{-# LANGUAGE OverloadedStrings #-}

-- | Backend-free MessagePack RPC Client.
module Network.MessagePack.RPC.Client
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
    , shutdown
    -- * Call and reply
    , Result
    , call
    , reply
    )
where

import           Control.Concurrent      (ThreadId, forkFinally, forkIO,
                                          killThread)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import qualified Control.Exception.Safe  as E
import           Control.Monad           (forever, void, when)
import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as BL
import           Data.HashMap.Strict     (HashMap)
import qualified Data.HashMap.Strict     as HM
import           Data.IORef              (IORef)
import qualified Data.IORef              as IORef
import qualified Data.MessagePack        as MsgPack
import           Data.Monoid             ((<>))
import           System.Timeout          (timeout)

import           Data.MessagePack.RPC

-- | A client data type for MessagePack RPC.
data Client = Client {
    clientSessionState :: !SessionState
  , clientBackend      :: !Backend
  , clientLog          :: Logger
  , clientFormat       :: Formatter
  , clientHandlerTid   :: IORef (Maybe ThreadId)
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
  , requestHandler      :: RequestHandler
  , logger              :: Logger
  , exceptionHandlers   :: [E.Handler IO ()]
    -- ^ Handles an exception thrown from the receiver thread,
    --   which is the only thread to receive 'Message's via 'Backend'.
    --   Until exiting from the block of 'withClient', the receiver thread
    --   indefinitely waits for frames via 'backendRecv'.
  , formatter           :: Formatter
  , waitRequestHandler  :: Bool
  }

-- | The default configuration.
--    'formatter' is 'show'.
--     Others do nothing.
defaultConfig :: Config
defaultConfig = Config
    { notificationHandler = \_ _ _ -> return ()
    , requestHandler      = \_ _ _ _ -> return ()
    , logger              = \_ -> return ()
    , exceptionHandlers   = [E.Handler $ \(E.SomeException _) -> return ()]
    , formatter           = show
    , waitRequestHandler  = False
    }

-- | Backend IO functions.
data Backend = Backend {
    backendSend  :: B.ByteString -> IO () -- ^ Sending
  , backendRecv  :: IO B.ByteString -- ^ Receiving
  , backendClose :: IO () -- ^ Closing
  }

-- TODO: Returns any exception
-- | Calling RPC.
call :: Client -> MethodName -> [MsgPack.Object] -> IO Result
call client funName args = do
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
reply :: Client -> MessageId -> Result -> IO ()
reply client mid result = do
    let response = ResponseMessage mid result
    let p        = BL.toStrict $ MsgPack.pack response
    backendSend (clientBackend client) p
    clientLog client $ "sent: " <> clientFormat client response

getNewMessageId :: SessionState -> IO MessageId
getNewMessageId ss =
    IORef.atomicModifyIORef (lastMessageId ss) $ \cur -> (cur + 1, cur)

receiverThread :: Client -> Config -> IO ()
receiverThread client config =
    (`E.catches` exceptionHandlers config) $ forever $ do
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
                void . forkIO $ notificationHandler config
                                                    client
                                                    methodName
                                                    params
            RequestMessage mid methodName params ->
                void . forkIO $ requestHandler config
                                               client
                                               mid
                                               methodName
                                               params
    where ss = clientSessionState client

initSessionState :: IO SessionState
initSessionState =
    SessionState <$> IORef.newIORef 0 <*> IORef.newIORef HM.empty

-- | Executing the action in the 3rd argument with a 'Client'.
withClient :: Config -> Backend -> (Client -> IO a) -> IO a
withClient config backend action = do
    ss     <- initSessionState
    wait   <- MVar.newEmptyMVar
    tidref <- IORef.newIORef Nothing
    let client = Client ss backend (logger config) (formatter config) tidref
    tid <- forkFinally (receiverThread client config)
        $ \_ -> MVar.putMVar wait ()
    IORef.writeIORef tidref $ Just tid
    takeAction client wait `E.finally` killThread tid
  where
    takeAction client wait = do
        returned <- action client
        when (waitRequestHandler config) $ MVar.takeMVar wait
        backendClose backend
        return returned

shutdown :: Client -> IO ()
shutdown client = do
    mtid <- IORef.readIORef $ clientHandlerTid client
    case mtid of
        Nothing  -> return ()
        Just tid -> killThread tid
