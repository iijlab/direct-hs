{-# LANGUAGE OverloadedStrings #-}

-- | Implementation of [MessagePack RPC](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md)
--   via WebSocket.
module Network.MessagePack.ClientViaWebSockets
  ( Client
  , Config(notificationHandler, requestHandler)
  , NotificationHandler
  , RequestHandler
  , EndpointUrl
  , defaultConfig
  , withClient
  , callRpc
  , replyRpc
  , getNewMessageId
  , forkReceiverThread
  , initSessionState
  , parseWsUrl
  ) where


import           Control.Concurrent (ThreadId, killThread, forkIO)
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
import qualified Control.Error as Err
import qualified Control.Exception as E
import           Control.Monad (forever, join)
import qualified Data.ByteString.Lazy as B
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (fromMaybe)
import           Data.MessagePack (MessagePack)
import qualified Data.MessagePack as MsgPack
import qualified Data.Text as T
import           Data.Word (Word64)
import           Network.Socket (withSocketsDo, PortNumber)
import qualified Network.WebSockets as Ws
import           Network.URI (parseURI, URI(..), URIAuth(..))
import qualified Numeric
import           Text.Read (readMaybe)
import qualified Wuss as Wss

import           Data.MessagePack.RPC

import           Debug.Trace

data Client =
  Client
    { clientSend  :: B.ByteString -> IO ()
    , clientRecv  :: IO B.ByteString
    , clientCLose :: B.ByteString -> IO () -- fixme
    , clientSessionState :: !SessionState
    }

data SessionState =
  SessionState
    { lastMessageId :: TVar MessageId
    , responseBuffer :: TVar (HashMap MessageId (TMVar (Either MsgPack.Object MsgPack.Object)))
    -- ^ MessageId をキーとて、レスポンス(MsgPack.Object)を置くための箱を持つ
    }

data EndpointUrl =
  EndpointUrl
    { _endpointUrlIsSecure :: !Bool
    , _endpointUrlHost :: !String
    , _endpointUrlPath :: !String
    , _endpointUrlPort :: !PortNumber
    } deriving (Eq, Show)

data Config =
  Config
   { notificationHandler :: NotificationHandler
   , requestHandler :: RequestHandler
   }

defaultConfig :: Config
defaultConfig = Config (\_ _ _ -> return ()) (\_ _ _ _ -> return ())

type NotificationHandler = Client -> MethodName -> [MsgPack.Object] -> IO ()

type RequestHandler = Client -> MessageId -> MethodName -> [MsgPack.Object] -> IO ()

withClient :: EndpointUrl -> Config -> (Client -> IO a) -> IO a
withClient (EndpointUrl sec host path port) config action =
  withSocketsDo $ runWs $ \conn -> do
    Ws.forkPingThread conn 30
    ss <- initSessionState
    let c = Client (Ws.sendBinaryData conn) (Ws.receiveData conn) (Ws.sendClose conn) ss
    tid <- forkReceiverThread c config
    ( do
      returned <- action c
      Ws.sendClose conn ("Bye!" :: T.Text)
      return returned
      ) `E.finally` killThread tid
  where
    runWs =
      if sec
        then Wss.runSecureClient host port path
        else Ws.runClient host (fromIntegral port) path


-- TODO: (DONE): Wait response
-- TODO: (DONE): Thread to write response
-- TODO: May need to lock connection before sending (Is Ws.Connection threadsafe?)
-- TODO: Returns any exception
callRpc
  :: Client
  -> T.Text
  -> [MsgPack.Object]
  -> IO (Either MsgPack.Object MsgPack.Object)
callRpc client funName args = do
  let st = clientSessionState client
      magicNumber = MsgPack.ObjectWord 0
  (requestId, resBuf) <- getNewMessageId st
  putStr "Function name: "
  print funName
  putStr "Arguments: "
  print args
  putStr "Payload: "
  let p = MsgPack.pack
        [ magicNumber
        , MsgPack.ObjectWord requestId
        , MsgPack.ObjectStr funName
        , MsgPack.ObjectArray args
        ]
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
  let magicNumber = MsgPack.ObjectWord 1
      p = MsgPack.pack
        [ magicNumber
        , MsgPack.ObjectWord mid
        , MsgPack.ObjectNil
        , result
        ]
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


parseWsUrl :: String -> Either String EndpointUrl
parseWsUrl raw = do
  uri <- noteInvalidUrl "Invalid URL given" $ parseURI raw
  auth <- noteInvalidUrl "No authroity specified" $ uriAuthority uri
  host <- dieWhenEmpty "No host specified" $ uriRegName auth
  let path = uriPath uri
      wss = "wss:"
      scheme' = uriScheme uri
      scheme = if null scheme' then wss else scheme'
      isSecure = scheme == wss
      defaultPort = if isSecure then 443 else 80
  return $
    EndpointUrl
      isSecure
      host
      (path ++ uriQuery uri)
      ( fromMaybe defaultPort
          $ readMaybe
          $ drop 1 {- drop the first colon -}
          $ uriPort auth
      )

  where
    noteInvalidUrl :: String -> Maybe a -> Either String a
    noteInvalidUrl msg = Err.note (msg ++ ": " ++ show raw)

    dieWhenEmpty :: String -> String -> Either String String
    dieWhenEmpty msg "" = Left (msg ++ ": " ++ show raw)
    dieWhenEmpty _ s = return s
