{-# LANGUAGE OverloadedStrings #-}

-- | MessagePack RPC Client.
module Network.MessagePack.Async.Client
  (
    -- * Config
    Config(..)
  , NotificationHandler
  , RequestHandler
  , Logger
  , Formatter
  , defaultConfig
    -- * Backend
  , Backend(..)
    -- * Client
  , Client
  , withClient
    -- * Call and reply
  , Result
  , callRpc
  , replyRpc
  ) where

import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import           Data.Monoid ((<>))
import qualified Control.Exception as E
import           Control.Monad (forever, void)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.MessagePack as MsgPack
import           System.Timeout (timeout)

import           Data.MessagePack.RPC

-- | A client data type for MessagePack RPC.
data Client = Client {
    clientSessionState :: !SessionState
  , clientBackend      :: !Backend
  , clientLog          :: Logger
  , clientFormat       :: Formatter
  }

data SessionState = SessionState {
    lastMessageId :: IORef MessageId
  , dispatchTable :: IORef (HashMap MessageId (MVar Result))
  }

-- | Result type of a RPC call.
--   Described as "error" and "result" of "Response Message"
--   in [the spec of MessagePack RPC](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md#response-message).
type Result = Either MsgPack.Object MsgPack.Object

-- | Notification handler. The 3rd argument is response objects.
type NotificationHandler = Client -> MethodName -> [MsgPack.Object] -> IO ()

-- | Notification handler. The 2nd argument is message id to be used
--   for replying. The 3rd argument is response objects.
type RequestHandler = Client -> MessageId -> MethodName -> [MsgPack.Object] -> IO ()

-- | Logger type. Should print out the message passed as a first argument somewhere.
type Logger = String -> IO ()

-- | Convert 'Message' into a @String@ to print out by 'Logger'
type Formatter = Message -> String

-- | Configuration for MessagePack RPC.
data Config = Config {
    notificationHandler :: NotificationHandler
  , requestHandler :: RequestHandler
  , logger :: Logger
  , formatter :: Formatter
  }

-- | The default configuration.
--    'formatter' is 'show'.
--     Others do nothing.
defaultConfig :: Config
defaultConfig = Config
  { notificationHandler = \_ _ _ -> return ()
  , requestHandler      = \_ _ _ _ -> return ()
  , logger              = \_ -> return ()
  , formatter           = show
  }

-- | Backend IO functions.
data Backend = Backend {
    backendSend :: B.ByteString -> IO () -- ^ Sending
  , backendRecv :: IO B.ByteString -- ^ Receiving
  , backendClose :: IO () -- ^ Closing
  }

-- TODO: May need to lock connection before sending (Is Ws.Connection threadsafe?)
-- TODO: Returns any exception
-- | Calling RPC.
callRpc :: Client -> MethodName -> [MsgPack.Object] -> IO Result
callRpc client funName args = do
  rrsp <- E.bracket register unregister sendAndRecv
  case rrsp of
    Nothing  -> return $ Left MsgPack.ObjectNil
    Just rsp -> return rsp
 where
  sendAndRecv (requestId, rspVar) = do
    let request = RequestMessage requestId funName args
    backendSend (clientBackend client) $ BL.toStrict $ MsgPack.pack request
    clientLog client $ "sent: " <> clientFormat client request
    timeout 3000000 $ MVar.takeMVar rspVar
  register = do
    requestId <- getNewMessageId ss
    rspVar    <- MVar.newEmptyMVar
    IORef.atomicModifyIORef' (dispatchTable ss)
      $ \tbl -> (HM.insert requestId rspVar tbl, ())
    return (requestId, rspVar)
  unregister (requestId, _) = IORef.atomicModifyIORef' (dispatchTable ss)
    $ \tbl -> (HM.delete requestId tbl, ())
  ss = clientSessionState client

-- | Replying RPC. This should be used in 'RequestHandler'.
replyRpc :: Client -> MessageId -> Result -> IO ()
replyRpc client mid result = do
  let response = ResponseMessage mid result
  let p        = BL.toStrict $ MsgPack.pack response
  backendSend (clientBackend client) p
  clientLog client $ "sent: " <> clientFormat client response

getNewMessageId :: SessionState -> IO MessageId
getNewMessageId ss =
  IORef.atomicModifyIORef (lastMessageId ss) $ \cur -> (cur + 1, cur)

receiverThread :: Client -> Config -> IO ()
receiverThread client config =
  E.handle (\(E.SomeException e) -> print e) $ forever $ do
    response <- MsgPack.unpack . BL.fromStrict =<< backendRecv
      (clientBackend client)
    clientLog client $ "received: " <> clientFormat client response
    case response of
      ResponseMessage mid result -> do
        tbl <- IORef.readIORef $ dispatchTable ss
        case HM.lookup mid tbl of
          Just rspVar -> MVar.putMVar rspVar result
          Nothing ->
            clientLog client
              $  "ERROR: No MVar assinged with request ID "
              ++ show mid
              ++ "."
      NotificationMessage methodName params ->
        void . forkIO $ notificationHandler config client methodName params
      RequestMessage mid methodName params ->
        void . forkIO $ requestHandler config client mid methodName params
  where ss = clientSessionState client

initSessionState :: IO SessionState
initSessionState =
  SessionState <$> IORef.newIORef 0 <*> IORef.newIORef HM.empty

-- | Executing the action in the 3rd argument with a 'Client'.
withClient :: Config -> Backend -> (Client -> IO a) -> IO a
withClient config backend action = do
  ss <- initSessionState
  let client = Client ss backend (logger config) (formatter config)
  tid <- forkIO $ receiverThread client config
  takeAction client `E.finally` killThread tid
 where
  takeAction client = do
    returned <- action client
    backendClose backend
    return returned
