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
    , dispatchTable :: TVar (HashMap MessageId (TMVar Result))
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
  let st = clientSessionState client
  (requestId, resBuf) <- getNewMessageId st
  let request = RequestMessage requestId funName args
  backendSend (clientBackend client) $ MsgPack.pack request
  clientLog client "sent" request

  atomically $ do
    res <- takeTMVar resBuf
    let dispatchTableVar = dispatchTable st
    modifyTVar' dispatchTableVar $ HM.delete requestId
    return res


-- | Replying RPC. This should be used in 'RequestHandler'.
replyRpc :: Client -> MessageId -> Result -> IO ()
replyRpc client mid result = do
  let response = ResponseMessage mid result
  let p = MsgPack.pack response
  backendSend (clientBackend client) p
  clientLog client "sent" response

getNewMessageId :: SessionState -> IO (MessageId, TMVar Result)
getNewMessageId ss = atomically $ do
  let lastMessageIdVar = lastMessageId ss
      dispatchTableVar = dispatchTable ss

  current <- readTVar lastMessageIdVar
  modifyTVar' lastMessageIdVar (+ 1)

  tmv <- newEmptyTMVar
  modifyTVar' dispatchTableVar $ HM.insert current tmv

  return (current, tmv)

receiverThread :: Client -> Config -> IO ()
receiverThread client config = E.handle (\(E.SomeException e) -> print e) $ forever $ do
    response <- MsgPack.unpack =<< backendRecv (clientBackend client)
    clientLog client "received" response
    case response of
      ResponseMessage mid result ->
        join $ atomically $ do
          resBuf <- readTVar $ dispatchTable ss
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
  where
    ss = clientSessionState client

initSessionState :: IO SessionState
initSessionState = SessionState <$> newTVarIO 0 <*> newTVarIO HM.empty

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
