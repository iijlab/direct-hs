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
  , Exception(..)
  , DirectInt64
  , TalkId
  -- * To be obsoleted
  , createMessage
  ) where


import           Control.Error (fmapL)
import qualified Control.Exception as E
import           Control.Monad (forM_, void)
import qualified Data.MessagePack as MsgPack
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
defaultConfig = Rpc.defaultConfig { Rpc.requestHandler = defaultRequestHandler }

defaultRequestHandler :: Rpc.RequestHandler
defaultRequestHandler c mid _methodName _objs =
  Rpc.replyRpc c mid $ Right $ MsgPack.ObjectBool True



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


createMessage :: Client -> TalkId -> TL.Text -> IO ()
createMessage c tid content = do
  let messageType = MsgPack.ObjectWord 1
  -- NOTE:
  --  direct-js internally splits the message by 1024 characters.
  --  So this library follows the behavior.
  forM_ (TL.chunksOf 1024 content) $ \chunk ->
    rethrowingException $ Rpc.callRpc
      (clientRpcClient c)
      "create_message"
      [ MsgPack.toObject tid
      , messageType
      , MsgPack.ObjectStr $ TL.toStrict chunk
      ]

createSession :: Client -> IO ()
createSession c =
  void $ rethrowingException $ Rpc.callRpc
      (clientRpcClient c)
      "create_session"
      [ MsgPack.ObjectStr $ persistedInfoDirectAccessToken $ clientPersistedInfo c
      , MsgPack.ObjectStr apiVersion
      , MsgPack.ObjectStr agentName
      ]

login
  :: AnonymousClient
  -> T.Text -- ^ Login email address for direct.
  -> T.Text -- ^ Login password for direct.
  -> IO (Either Exception Client)
login c email pass = do
  idfv <- genIdfv

  let magicConstant = MsgPack.ObjectStr ""
  res <-
    Rpc.callRpc
      c
      "create_access_token"
      [ MsgPack.ObjectStr email
      , MsgPack.ObjectStr pass
      , MsgPack.ObjectStr idfv
      , MsgPack.ObjectStr agentName
      , magicConstant
      ]
  case extractResult res of
      Right (MsgPack.ObjectStr token) ->
        return $ Right $ Client
          (PersistedInfo token idfv)
          c
      Right other ->
        return $ Left $ UnexpectedReponse other
      Left e -> return $ Left e
  -- Example no error: ObjectArray [ObjectWord 1,ObjectWord 0,ObjectNil,ObjectStr "..."]
  -- Example error:    ObjectArray [ObjectWord 1,ObjectWord 0,ObjectMap [(ObjectStr "code",ObjectWord 401),(ObjectStr "message",ObjectStr "invalid email or password")],ObjectNil]


rethrowingException :: IO (Either MsgPack.Object MsgPack.Object) -> IO MsgPack.Object
rethrowingException action = do
  res <- action
  case extractResult res of
      Right obj -> return obj
      Left e -> E.throwIO e


extractResult :: Rpc.Result -> Either Exception MsgPack.Object
extractResult = fmapL $
  \case
    err@(MsgPack.ObjectMap errorMap) ->
      let isInvalidEP =
            lookup (MsgPack.ObjectStr "message") errorMap
              == Just (MsgPack.ObjectStr "invalid email or password")
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
