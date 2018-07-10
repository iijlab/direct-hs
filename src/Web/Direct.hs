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
  , Rpc.URL
  -- * Client
  , Client
  , withClient
  , clientPersistedInfo
  , setDomains
  , getDomains
  , setTalkRooms
  , getTalkRooms
  , setMe
  , getMe
  , setUsers
  , getUsers
  -- ** Persisted information
  , PersistedInfo(..)
  , serializePersistedInfo
  , deserializePersistedInfo
  -- * Message
  -- ** Ids
  , DomainId
  , TalkId
  , UserId
  , MessageId
  -- ** Main types
  , Domain(..)
  , TalkType(..)
  , TalkRoom(..)
  , User(..)
  , Message(..)
  , Aux(..)
  -- * Sending
  , sendMessage
  -- * Channel
  , Channel
  , withChannel
  , recv
  -- *Exceptions
  , Exception(..)
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

import qualified Network.MessagePack.Async.Client.WebSocket as Rpc
import           Web.Direct.Types

----------------------------------------------------------------

data Config = Config {
    directCreateMessageHandler :: Client -> Message -> Aux -> IO ()
  , directLogger               :: Rpc.Logger
  , directFormatter            :: Rpc.Formatter
  }

-- | The default configuration.
--   'RequestHandler' automatically replies ACK.
--   'NotificationHandler' and 'logger' do nothing.
--   'formatter' is 'show'.
defaultConfig :: Config
defaultConfig = Config
    { directCreateMessageHandler = \_ _ _ -> return ()
    , directLogger               = \_ -> return ()
    , directFormatter            = show
    }

----------------------------------------------------------------

login :: Config
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
            Right <$> newClient (PersistedInfo token idfv) client
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

extractResult :: Rpc.Result -> Either Exception M.Object
extractResult = fmapL $ \case
    err@(M.ObjectMap errorMap) ->
        let isInvalidEP = lookup (M.ObjectStr "message") errorMap
                == Just (M.ObjectStr "invalid email or password")
        in  if isInvalidEP
                then InvalidEmailOrPassword
                else UnexpectedReponse err
    other -> UnexpectedReponse other

----------------------------------------------------------------

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

----------------------------------------------------------------

withClient :: Config -> Rpc.URL -> PersistedInfo -> (Client -> IO a) -> IO a
withClient config url pInfo action = do
    ref <- I.newIORef Nothing
    Rpc.withClient url (rpcConfig ref) $ \rpcClient -> do
        client <- newClient pInfo rpcClient
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
                    Nothing        -> return ()
                    Just (msg, aux) -> do
                        ec <- findChannel client aux
                        case ec of
                            Just chan -> dispatch chan msg aux
                            Nothing   -> directCreateMessageHandler config client msg aux
                _ -> return ()
        , Rpc.logger         = directLogger config
        , Rpc.formatter      = directFormatter config
        }

createSession :: Client -> IO ()
createSession client = do
    ersp <- Rpc.callRpc (clientRpcClient client)
                       "create_session"
                       [ M.ObjectStr $ persistedInfoDirectAccessToken $ clientPersistedInfo client
                       , M.ObjectStr apiVersion
                       , M.ObjectStr agentName
                       ]
    case ersp of
        Right rsp -> case fromCreateSession rsp of
            Just user -> setMe client user
            Nothing   -> return ()
        _             -> return ()

subscribeNotification :: Client -> IO ()
subscribeNotification client = do
    let c = clientRpcClient client
    void $ rethrowingException $ Rpc.callRpc c "reset_notification" []
    void $ rethrowingException $ Rpc.callRpc c "start_notification" []
    Right doms <- Rpc.callRpc c "get_domains" []
    setDomains client $ fromGetDomains doms
    void $ rethrowingException $ Rpc.callRpc c "get_domain_invites" []
    void $ rethrowingException $ Rpc.callRpc c "get_account_control_requests" []
    void $ rethrowingException $ Rpc.callRpc c "get_joined_account_control_group" []
    void $ rethrowingException $ Rpc.callRpc c "get_announcement_statuses" []
    void $ rethrowingException $ Rpc.callRpc c "get_friends" []
    Right acq <- Rpc.callRpc c "get_acquaintances" []
    setUsers client $ fromGetAcquaintances acq
    Right talks <- Rpc.callRpc c "get_talks" []
    setTalkRooms client $ fromGetTalks talks
    void $ rethrowingException $ Rpc.callRpc c "get_talk_statuses" []

rethrowingException :: IO (Either M.Object M.Object) -> IO M.Object
rethrowingException action = do
    res <- action
    case extractResult res of
        Right obj -> return obj
        Left  e   -> E.throwIO e

----------------------------------------------------------------

sendAck :: Rpc.Client -> R.MessageId -> IO ()
sendAck rpcClient mid = Rpc.replyRpc rpcClient mid $ Right $ M.ObjectBool True

----------------------------------------------------------------

sendMessage :: Client -> Message -> Aux -> IO MessageId
sendMessage client req aux = do
    let obj = encodeMessage req aux
    ersp <- Rpc.callRpc (clientRpcClient client) "create_message" obj
    case ersp of
        Right (M.ObjectMap rsp) ->
            case lookup (M.ObjectStr "message_id") rsp of
                Just (M.ObjectWord x) -> return x
                _                     -> error "sendMessage" -- fixme
        _ -> error "sendMessage" -- fixme

----------------------------------------------------------------

withChannel :: Client -> Aux -> (Channel -> IO ()) -> IO ()
withChannel client aux body = E.bracket register unregister body
  where
    register = newChannel client aux
    unregister _ = freeChannel client aux
