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
  ) where

import           Control.Concurrent (ThreadId, forkIO, killThread)
import           Control.Concurrent.STM
                   ( TVar
                   , TMVar
                   , newTVarIO
                   , newEmptyTMVar
                   , modifyTVar'
                   , readTVar
                   , takeTMVar
                   , atomically
                   , putTMVar
                   )
import qualified Control.Exception as E
import           Control.Monad (forever, join)
import qualified Data.ByteString.Lazy as B
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.MessagePack as MsgPack

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
    { lastMessageId :: TVar MessageId
    , responseBuffer :: TVar (HashMap MessageId (TMVar (Either MsgPack.Object MsgPack.Object)))
    -- ^ MessageId をキーとて、レスポンス(MsgPack.Object)を置くための箱を持つ
    }

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

-- TODO: (DONE): Wait response
-- TODO: (DONE): Thread to write response
-- TODO: May need to lock connection before sending (Is Ws.Connection threadsafe?)
-- TODO: Returns any exception
-- | Calling RPC.
callRpc
  :: Client
  -> MethodName
  -> [MsgPack.Object]
  -> IO (Either MsgPack.Object MsgPack.Object)
callRpc client funName args = do
  let st = clientSessionState client
  (requestId, resBuf) <- getNewMessageId st
  let request = RequestMessage requestId funName args
  backendSend (clientBackend client) $ MsgPack.pack request
  clientLog client "sent" request

  atomically $ do
    res <- takeTMVar resBuf
    let responseBufferVar = responseBuffer st
    modifyTVar' responseBufferVar $ HM.delete requestId
    return res


-- TODO: Receive Either MsgPack.Object MsgPack.Object
-- | Replying RPC. This should be used in 'RequestHandler'.
replyRpc :: Client -> MessageId -> MsgPack.Object -> IO ()
replyRpc client mid result = do
  let response = ResponseMessage mid (Right result)
  let p = MsgPack.pack response
  backendSend (clientBackend client) p
  clientLog client "sent" response

getNewMessageId :: SessionState -> IO (MessageId, TMVar (Either MsgPack.Object MsgPack.Object))
getNewMessageId ss = atomically $ do
  let lastMessageIdVar = lastMessageId ss
      responseBufferVar = responseBuffer ss

  current <- readTVar lastMessageIdVar
  modifyTVar' lastMessageIdVar (+ 1)

  tmv <- newEmptyTMVar
  modifyTVar' responseBufferVar $ HM.insert current tmv

  return (current, tmv)

forkReceiverThread :: Client -> Config -> IO ThreadId
forkReceiverThread client config = forkIO $ do
  let ss = clientSessionState client
  forever $ do
    response <- MsgPack.unpack =<< backendRecv (clientBackend client)
    clientLog client "received" response
    case response of
      ResponseMessage mid result ->
        join $ atomically $ do
          resBuf <- readTVar $ responseBuffer ss
          case HM.lookup mid resBuf of
              Just tv -> do
                putTMVar tv result
                return $ return ()
              Nothing ->
                -- TODO: Use logging library
                return $
                  putStrLn $ "ERROR: No TVar assinged with request ID " ++ show mid ++ "."
      NotificationMessage methodName params -> do
        notificationHandler config client methodName params
      RequestMessage mid methodName params -> do
        requestHandler config client mid methodName params


initSessionState :: IO SessionState
initSessionState = SessionState <$> newTVarIO 0 <*> newTVarIO HM.empty

withClient :: Config -> Backend -> (Client -> IO a) -> IO a
withClient config backend action = do
    ss <- initSessionState
    let client = Client ss backend (logger config)
    tid <- forkReceiverThread client config
    takeAction client `E.finally` killThread tid
  where
    takeAction client = do
      returned <- action client
      backendClose backend
      return returned
