{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}

module Web.Direct
  (
    Config(..)
  , defaultConfig
  -- * Login
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
  -- ** Ids
  , DomainId
  , TalkId
  , UserId
  , MessageId
  -- ** Messages
  , Message(..)
  , messageTalkId
  -- ** Exceptions
  , Exception(..)
  -- * APIs
  , sendMessage
  ) where


import           Control.Error                              (fmapL)
import qualified Control.Exception                          as E
import           Control.Monad                              (void, when)
import qualified Data.IORef                                 as I
import qualified Data.MessagePack                           as M
import qualified Data.MessagePack.RPC                       as R
import qualified Data.Text                                  as T
import qualified Data.UUID                                  as Uuid
import qualified System.Random.MWC                          as Random

import           Web.Direct.Types

import qualified Network.MessagePack.Async.Client.WebSocket as Rpc

data Config = Config {
    directCreateMessageHandler :: Client -> Message -> IO ()
  , directLogger               :: Rpc.Logger
  , directFormatter            :: Rpc.Formatter
  }

-- | The default configuration.
--   'RequestHandler' automatically replies ACK.
--   'NotificationHandler' and 'logger' do nothing.
--   'formatter' is 'show'.
defaultConfig :: Config
defaultConfig = Config
    { directCreateMessageHandler = \_ _ -> return ()
    , directLogger               = \_ -> return ()
    , directFormatter            = show
    }

withClient :: Config -> Rpc.URL -> PersistedInfo -> (Client -> IO a) -> IO a
withClient config url pInfo action = do
    ref <- I.newIORef Nothing
    Rpc.withClient url (rpcConfig ref) $ \rpcClient -> do
        let client = Client pInfo rpcClient
        I.writeIORef ref $ Just client
        createSession client
        subscribeNotification client
        action client
  where
    rpcConfig ref = Rpc.defaultConfig
        { Rpc.requestHandler = \rpcClient mid method objs -> do
             -- sending ACK always
            sendAck rpcClient mid
            Just client <- I.readIORef ref
            -- fixme: "notify_update_domain_users"
            -- fixme: "notify_update_read_statuses"
            when (method == "notify_create_message") $ case objs of
                M.ObjectMap rsp : _ -> case decodeMessage rsp of
                    Nothing  -> return ()
                    Just req -> directCreateMessageHandler config client req
                _ -> return ()
        , Rpc.logger         = directLogger config
        , Rpc.formatter      = directFormatter config
        }

subscribeNotification :: Client -> IO ()
subscribeNotification client = do
    let c = clientRpcClient client
    void $ rethrowingException $ Rpc.callRpc c "reset_notification" []
    void $ rethrowingException $ Rpc.callRpc c "start_notification" []
    void $ rethrowingException $ Rpc.callRpc c "get_domains" []
    void $ rethrowingException $ Rpc.callRpc c "get_domain_invites" []
    void $ rethrowingException $ Rpc.callRpc c "get_account_control_requests" []
    void $ rethrowingException $ Rpc.callRpc c "get_joined_account_control_group" []
    void $ rethrowingException $ Rpc.callRpc c "get_announcement_statuses" []
    void $ rethrowingException $ Rpc.callRpc c "get_friends" []
    void $ rethrowingException $ Rpc.callRpc c "get_acquaintances" []
    void $ rethrowingException $ Rpc.callRpc c "get_talks" []
    void $ rethrowingException $ Rpc.callRpc c "get_talk_statuses" []


sendMessage :: Client -> Message -> IO MessageId
sendMessage c req = do
    let obj = encodeMessage req
    ersp <- Rpc.callRpc (clientRpcClient c) "create_message" obj
    case ersp of
        Right (M.ObjectMap rsp) ->
            case lookup (M.ObjectStr "message_id") rsp of
                Just (M.ObjectWord x) -> return x
                _                     -> error "sendMessage" -- fixme
        _ -> error "sendMessage" -- fixme

sendAck :: Rpc.Client -> R.MessageId -> IO ()
sendAck c mid = Rpc.replyRpc c mid $ Right $ M.ObjectBool True

createSession :: Client -> IO ()
createSession c = void $ rethrowingException $ Rpc.callRpc
    (clientRpcClient c)
    "create_session"
    [ M.ObjectStr $ persistedInfoDirectAccessToken $ clientPersistedInfo c
    , M.ObjectStr apiVersion
    , M.ObjectStr agentName
    ]

login
    :: Config
    -> Rpc.URL
    -> T.Text -- ^ Login email address for direct.
    -> T.Text -- ^ Login password for direct.
    -> IO (Either Exception Client)
login config url email pass = Rpc.withClient url rpcConfig $ \client -> do
    idfv <- genIdfv

    let magicConstant = M.ObjectStr ""
    res <- Rpc.callRpc
        client
        "create_access_token"
        [ M.ObjectStr email
        , M.ObjectStr pass
        , M.ObjectStr idfv
        , M.ObjectStr agentName
        , magicConstant
        ]
    case extractResult res of
        Right (M.ObjectStr token) ->
            return $ Right $ Client (PersistedInfo token idfv) client
        Right other -> return $ Left $ UnexpectedReponse other
        Left  e     -> return $ Left e
  where
    rpcConfig = Rpc.defaultConfig
        { Rpc.requestHandler = \rpcClient mid _method _objs -> do
             -- sending ACK always
            sendAck rpcClient mid
        , Rpc.logger         = directLogger config
        , Rpc.formatter      = directFormatter config
        }

rethrowingException :: IO (Either M.Object M.Object) -> IO M.Object
rethrowingException action = do
    res <- action
    case extractResult res of
        Right obj -> return obj
        Left  e   -> E.throwIO e


extractResult :: Rpc.Result -> Either Exception M.Object
extractResult = fmapL $ \case
    err@(M.ObjectMap errorMap) ->
        let isInvalidEP = lookup (M.ObjectStr "message") errorMap
                == Just (M.ObjectStr "invalid email or password")
        in  if isInvalidEP
                then InvalidEmailOrPassword
                else UnexpectedReponse err
    other -> UnexpectedReponse other


genIdfv :: IO T.Text
genIdfv = do
    g <- Random.createSystemRandom
    Uuid.toText
        <$> (   Uuid.fromWords
            <$> Random.uniform g
            <*> Random.uniform g
            <*> Random.uniform g
            <*> Random.uniform g
            )


agentName :: T.Text
agentName = "bot"


apiVersion :: T.Text
apiVersion = "1.91"
