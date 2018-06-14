{-# LANGUAGE OverloadedStrings #-}

module Network.MessagePack.Async.Client
  (
    -- * Client
    Client
  , newClient
    -- * Config
  , NotificationHandler
  , RequestHandler
  , Logger
  , Config(..)
  , defaultConfig
    -- * Call and reply
  , callRpc
  , replyRpc
    -- * Misc
  , forkReceiverThread
  ) where

import           Control.Concurrent (ThreadId, forkIO)
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
import           Control.Monad (forever, join)
import qualified Data.ByteString.Lazy as B
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import qualified Data.MessagePack as MsgPack

import           Data.MessagePack.RPC

-- | A client data type for MessagePack RPC.
data Client =
  Client
    { clientSend  :: B.ByteString -> IO ()
    , clientRecv  :: IO B.ByteString
    , clientSessionState :: !SessionState
    , clientLog   :: String -> Message -> IO ()
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
  clientLog client "request" request
  clientSend client $ MsgPack.pack request
  atomically $ do -- TODO: Split out as a function
    res <- takeTMVar resBuf
    let responseBufferVar = responseBuffer st
    modifyTVar' responseBufferVar $ HM.delete requestId
    return res


-- TODO: Receive Either MsgPack.Object MsgPack.Object
-- | Replying RPC. This should be used in 'RequestHandler'.
replyRpc :: Client -> MessageId -> MsgPack.Object -> IO ()
replyRpc client mid result = do
  let p = MsgPack.pack $ ResponseMessage mid (Right result)
  clientSend client p

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
forkReceiverThread c config = forkIO $ do
  let ss = clientSessionState c
  forever $ do
    response <- MsgPack.unpack =<< clientRecv c
    clientLog c "response" response
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
        notificationHandler config c methodName params
      RequestMessage mid methodName params -> do
        requestHandler config c mid methodName params


initSessionState :: IO SessionState
initSessionState = SessionState <$> newTVarIO 0 <*> newTVarIO HM.empty

-- | Creating a new client of MessagePack RPC.
newClient :: Config -> (B.ByteString -> IO ()) -> IO B.ByteString -> IO Client
newClient config send recv = do
    ss <- initSessionState
    return $ Client send recv ss (logger config)
