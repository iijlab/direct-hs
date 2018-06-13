{-# LANGUAGE OverloadedStrings #-}

module Network.MessagePack.Async.Client
  (
    -- * Client
    Client(..)
    -- * Config
  , Config(..)
  , NotificationHandler
  , RequestHandler
  , defaultConfig
    -- * Call and reply
  , callRpc
  , replyRpc
    -- * Session state
  , SessionState
  , initSessionState
    -- * Misc
  , getNewMessageId
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
import           Network.Socket (PortNumber)
import qualified Numeric

import           Data.MessagePack.RPC

import           Debug.Trace

data Client =
  Client
    { clientSend  :: B.ByteString -> IO ()
    , clientRecv  :: IO B.ByteString
    , clientSessionState :: !SessionState
    }

data SessionState =
  SessionState
    { lastMessageId :: TVar MessageId
    , responseBuffer :: TVar (HashMap MessageId (TMVar (Either MsgPack.Object MsgPack.Object)))
    -- ^ MessageId をキーとて、レスポンス(MsgPack.Object)を置くための箱を持つ
    }

data Config =
  Config
   { notificationHandler :: NotificationHandler
   , requestHandler :: RequestHandler
   }

defaultConfig :: Config
defaultConfig = Config (\_ _ _ -> return ()) (\_ _ _ _ -> return ())

type NotificationHandler = Client -> MethodName -> [MsgPack.Object] -> IO ()

type RequestHandler = Client -> MessageId -> MethodName -> [MsgPack.Object] -> IO ()

-- TODO: (DONE): Wait response
-- TODO: (DONE): Thread to write response
-- TODO: May need to lock connection before sending (Is Ws.Connection threadsafe?)
-- TODO: Returns any exception
callRpc
  :: Client
  -> MethodName
  -> [MsgPack.Object]
  -> IO (Either MsgPack.Object MsgPack.Object)
callRpc client funName args = do
  let st = clientSessionState client
  (requestId, resBuf) <- getNewMessageId st
  putStr "Function name: "
  print funName
  putStr "Arguments: "
  print args
  putStr "Payload: "
  let p = MsgPack.pack $ RequestMessage requestId funName args
  putStrLn $ unwords $ map (($ "") . Numeric.showHex) $ B.unpack p
  clientSend client p
  atomically $ do -- TODO: Split out as a function
    res <- takeTMVar resBuf
    let responseBufferVar = responseBuffer st
    modifyTVar' responseBufferVar $ HM.delete requestId
    return res


-- TODO: Receive Either MsgPack.Object MsgPack.Object
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
    traceM $ "response: " ++ show response
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
        traceM "BEGIN Calling handler"
        notificationHandler config c methodName params
        traceM "FINISHED Calling handler"
      RequestMessage mid methodName params -> do
        requestHandler config c mid methodName params


initSessionState :: IO SessionState
initSessionState = SessionState <$> newTVarIO 0 <*> newTVarIO HM.empty
