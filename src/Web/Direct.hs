{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Web.Direct
  (
    Rpc.Config(..)
  , defaultConfig
  -- * Client not logined yet.
  , AnonymousClient
  , withAnonymousClient
  , login
  -- * Client
  , Client
  , withClient
  , clientPersistedInfo
  -- ** Persisted information
  , PersistedInfo(..)
  , serializePersistedInfo
  , deserializePersistedInfo
  -- * Types
  , Request(..)
  , Response(..)
  , RspInfo
  , Exception(..)
  , DirectInt64
  , TalkId
  -- * Functions
  , withResponse
  , sendRequest
  , replyAck
  -- * To be obsoleted
  , createMessage
  ) where


import           Control.Error (fmapL)
import qualified Control.Exception as E
import           Control.Monad (forM_, void)
import qualified Data.MessagePack as M
import qualified Data.MessagePack.RPC as R
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as Uuid
import qualified System.Random.MWC as Random

import           Web.Direct.Types

import qualified Network.MessagePack.Async.Client.WebSocket as Rpc

-- | The default configuration.
--   'RequestHandler' automatically replies ACK.
--   'NotificationHandler' and 'logger' do nothing.
--   'formatter' is 'show'.
defaultConfig :: Rpc.Config
defaultConfig = Rpc.defaultConfig {
    Rpc.requestHandler = \c i _ _ -> replyAck c i
  }


withClient
  :: String -> PersistedInfo -> Rpc.Config -> (Client -> IO a) -> IO a
withClient ep pInfo handler action = withAnonymousClient ep handler $ \aClient -> do
  let client = Client pInfo aClient
  createSession client
  subscribeNotification client
  action client


withAnonymousClient
  :: String -> Rpc.Config -> (AnonymousClient -> IO a) -> IO a
withAnonymousClient = Rpc.withClient


subscribeNotification :: Client -> IO ()
subscribeNotification client = do
  let c = clientRpcClient client
  void $ rethrowingException $ Rpc.callRpc c "reset_notification" []
  void $ rethrowingException $ Rpc.callRpc c "start_notification" []


withResponse :: [M.Object] -> (Response -> RspInfo -> IO ()) -> IO ()
withResponse (M.ObjectMap rspinfo:_) action = case decodeResponse rspinfo of
    Nothing   -> return ()
    Just req  -> action req rspinfo
withResponse _ _ = return ()

sendRequest :: Rpc.Client -> TalkId -> Request -> IO ()
sendRequest c tid req = do
    let obj = M.toObject tid : encodeRequest req
    void $ Rpc.callRpc c "create_message" obj

replyAck :: Rpc.Client -> R.MessageId -> IO ()
replyAck c mid  = Rpc.replyRpc c mid $ Right $ M.ObjectBool True

createMessage :: Client -> TalkId -> TL.Text -> IO ()
createMessage c tid content = do
  let messageType = M.ObjectWord 1
  -- NOTE:
  --  direct-js internally splits the message by 1024 characters.
  --  So this library follows the behavior.
  forM_ (TL.chunksOf 1024 content) $ \chunk ->
    rethrowingException $ Rpc.callRpc
      (clientRpcClient c)
      "create_message"
      [ M.toObject tid
      , messageType
      , M.ObjectStr $ TL.toStrict chunk
      ]

createSession :: Client -> IO ()
createSession c =
  void $ rethrowingException $ Rpc.callRpc
      (clientRpcClient c)
      "create_session"
      [ M.ObjectStr $ persistedInfoDirectAccessToken $ clientPersistedInfo c
      , M.ObjectStr apiVersion
      , M.ObjectStr agentName
      ]

login
  :: AnonymousClient
  -> T.Text -- ^ Login email address for direct.
  -> T.Text -- ^ Login password for direct.
  -> IO (Either Exception Client)
login c email pass = do
  idfv <- genIdfv

  let magicConstant = M.ObjectStr ""
  res <-
    Rpc.callRpc
      c
      "create_access_token"
      [ M.ObjectStr email
      , M.ObjectStr pass
      , M.ObjectStr idfv
      , M.ObjectStr agentName
      , magicConstant
      ]
  case extractResult res of
      Right (M.ObjectStr token) ->
        return $ Right $ Client
          (PersistedInfo token idfv)
          c
      Right other ->
        return $ Left $ UnexpectedReponse other
      Left e -> return $ Left e
  -- Example no error: ObjectArray [ObjectWord 1,ObjectWord 0,ObjectNil,ObjectStr "..."]
  -- Example error:    ObjectArray [ObjectWord 1,ObjectWord 0,ObjectMap [(ObjectStr "code",ObjectWord 401),(ObjectStr "message",ObjectStr "invalid email or password")],ObjectNil]


rethrowingException :: IO (Either M.Object M.Object) -> IO M.Object
rethrowingException action = do
  res <- action
  case extractResult res of
      Right obj -> return obj
      Left e -> E.throwIO e


extractResult :: Rpc.Result -> Either Exception M.Object
extractResult = fmapL $
  \case
    err@(M.ObjectMap errorMap) ->
      let isInvalidEP =
            lookup (M.ObjectStr "message") errorMap
              == Just (M.ObjectStr "invalid email or password")
      in
        if isInvalidEP
          then InvalidEmailOrPassword
          else UnexpectedReponse err
    other -> UnexpectedReponse other


genIdfv :: IO T.Text
genIdfv = do
  g <- Random.createSystemRandom
  Uuid.toText
    <$> (
      Uuid.fromWords
        <$> Random.uniform g
        <*> Random.uniform g
        <*> Random.uniform g
        <*> Random.uniform g
    )


agentName :: T.Text
agentName = "bot"


apiVersion :: T.Text
apiVersion = "1.91"
