{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Web.Direct
  ( AnonymousClient
  , Client
  , clientPersistedInfo

  , withAnonymousClient
  , withClient

  , PersistedInfo(..)
  , serializePersistedInfo
  , deserializePersistedInfo

  , Exception(..)

  , DirectInt64
  , TalkId

  , login
  , createMessage

  , Rpc.parseWsUrl
  ) where


import           Control.Error (fmapL)
import qualified Control.Exception as E
import           Control.Monad (forM, void)
import qualified Data.MessagePack as MsgPack
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.UUID as Uuid
import qualified System.Random.MWC as Random

import           Web.Direct.Types
import qualified Network.MessagePack.ClientViaWebSockets as Rpc


withClient
  :: Rpc.EndpointUrl -> PersistedInfo -> Rpc.NotificationHandler -> (Client -> IO a) -> IO a
withClient ep pInfo handler action = withAnonymousClient ep handler $ \aClient -> do
  let client = Client pInfo aClient
  createSession client
  subscribeNotification client
  action client


withAnonymousClient
  :: Rpc.EndpointUrl -> Rpc.NotificationHandler -> (AnonymousClient -> IO a) -> IO a
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
  res <- forM (TL.chunksOf 1024 content) $ \chunk -> do
    rethrowingException $ Rpc.callRpc
      (clientRpcClient c)
      "create_message"
      [ MsgPack.toObject tid
      , messageType
      , MsgPack.ObjectStr $ TL.toStrict chunk
      ]
  -- TODO: Define type for the response, then delete the debug message
  putStrLn $ "Successfully sent a message. ResponseRecieved: " ++ show res


createSession :: Client -> IO ()
createSession c = do
  res <-
    rethrowingException $ Rpc.callRpc
      (clientRpcClient c)
      "create_session"
      [ MsgPack.ObjectStr $ persistedInfoDirectAccessToken $ clientPersistedInfo c
      , MsgPack.ObjectStr apiVersion
      , MsgPack.ObjectStr agentName
      ]
  -- TODO: Where to save login user info
  putStrLn $ "Successfully created a session. ResponseRecieved: " ++ show res


login
  :: Rpc.Client
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


extractResult :: Either MsgPack.Object MsgPack.Object -> Either Exception MsgPack.Object
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
