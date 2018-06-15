{-# LANGUAGE OverloadedStrings #-}

module Network.MessagePack.Async.Client
  (
    -- * Config
    NotificationHandler
  , RequestHandler
  , Logger
  , Config(..)
  , defaultConfig
    -- * Backend
  , Backend(..)
    -- * Client
  , Client
  , withClient
    -- * Call and reply
  , callRpc
  , replyRpc

    -- * Other utility type synonym
  , Result
  ) where

import           Control.Concurrent (forkIO, killThread)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Data.IORef (IORef)
import qualified Data.IORef as IORef
import qualified Control.Exception as E
import           Control.Monad (forever, void)
import qualified Data.ByteString.Lazy as B
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.MessagePack as MsgPack
import           System.Timeout (timeout)

import           Data.MessagePack.RPC

-- | A client data type for MessagePack RPC.
data Client =
  Client
    { clientSessionState :: !SessionState
    , clientBackend      :: !Backend
    , clientLog          :: String -> Message -> IO ()
    }

data SessionState =
  SessionState
    { lastMessageId :: IORef MessageId
    , dispatchTable :: IORef (HashMap MessageId (MVar Result))
    -- ^ MessageId をキーとて、レスポンス(MsgPack.Object)を置くための箱を持つ
    }

-- | Result type of a RPC call.
--   Described as "error" and "result" of "Response Message"
--   in [the spec of MessagePack RPC](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md#response-message).
type Result = Either MsgPack.Object MsgPack.Object

type NotificationHandler = Client -> MethodName -> [MsgPack.Object] -> IO ()

type RequestHandler = Client -> MessageId -> MethodName -> [MsgPack.Object] -> IO ()

-- | Logger type. The first argument is a tag.
type Logger = String -> Message -> IO ()

data Config =
  Config
   { notificationHandler :: NotificationHandler
   , requestHandler :: RequestHandler
   , logger :: Logger
   }

-- | The default configuration. No action at all.
defaultConfig :: Config
defaultConfig = Config {
    notificationHandler = \_ _ _ -> return ()
  , requestHandler      = \_ _ _ _ -> return ()
  , logger              = \_ _ -> return ()
  }

-- fixme: we should use strict ByteString
data Backend = Backend {
    backendSend :: B.ByteString -> IO ()
  , backendRecv :: IO B.ByteString
  , backendClose :: IO ()
  }

-- TODO: May need to lock connection before sending (Is Ws.Connection threadsafe?)
-- TODO: Returns any exception
-- | Calling RPC.
callRpc
  :: Client
  -> MethodName
  -> [MsgPack.Object]
  -> IO Result
callRpc client funName args = do
  let ss = clientSessionState client
  requestId <- getNewMessageId ss
  rspVar <- MVar.newEmptyMVar
  let table = dispatchTable ss
  IORef.atomicModifyIORef' table $ \tbl ->
      (HM.insert requestId rspVar tbl, ())
  let request = RequestMessage requestId funName args
  backendSend (clientBackend client) $ MsgPack.pack request
  clientLog client "sent" request
  putStrLn $ "waiting for " ++ show requestId ++ "..."
  rrsp <- timeout 3000000 $ MVar.takeMVar rspVar
  IORef.atomicModifyIORef' table $ \tbl -> (HM.delete requestId tbl, ())
  case rrsp of
      Nothing  -> do
        putStrLn $ "waiting for " ++ show requestId ++ "... failed"
        return $ Left MsgPack.ObjectNil
      Just rsp -> do
        putStrLn $ "waiting for " ++ show requestId ++ "... done"
        return rsp

-- | Replying RPC. This should be used in 'RequestHandler'.
replyRpc :: Client -> MessageId -> Result -> IO ()
replyRpc client mid result = do
  let response = ResponseMessage mid result
  let p = MsgPack.pack response
  backendSend (clientBackend client) p
  clientLog client "sent" response

getNewMessageId :: SessionState -> IO MessageId
getNewMessageId ss = IORef.atomicModifyIORef (lastMessageId ss) $ \cur -> (cur + 1, cur)

receiverThread :: Client -> Config -> IO ()
receiverThread client config = E.handle (\(E.SomeException e) -> print e) $ forever $ do
    response <- MsgPack.unpack =<< backendRecv (clientBackend client)
    clientLog client "received" response
    case response of
      ResponseMessage mid result -> do
          tbl <- IORef.readIORef $ dispatchTable ss
          case HM.lookup mid tbl of
              Just rspVar -> MVar.putMVar rspVar result
              Nothing     -> putStrLn $ "ERROR: No MVar assinged with request ID " ++ show mid ++ "."
      NotificationMessage methodName params -> void . forkIO $
        notificationHandler config client methodName params
      RequestMessage mid methodName params -> void . forkIO $
        requestHandler config client mid methodName params
  where
    ss = clientSessionState client

initSessionState :: IO SessionState
initSessionState = SessionState <$> IORef.newIORef 0 <*> IORef.newIORef HM.empty

withClient :: Config -> Backend -> (Client -> IO a) -> IO a
withClient config backend action = do
    ss <- initSessionState
    let client = Client ss backend (logger config)
    tid <- forkIO $ receiverThread client config
    takeAction client `E.finally` killThread tid
  where
    takeAction client = do
      returned <- action client
      backendClose backend
      return returned
