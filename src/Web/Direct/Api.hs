{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Api
  (
    Config(..)
  , defaultConfig
  , login
  , RPC.URL
  , withClient
  ) where

import qualified Control.Exception                        as E
import           Control.Monad                            (mapM_, void, when)
import qualified Data.IORef                               as I
import qualified Data.MessagePack                         as M
import qualified Data.MessagePack.RPC                     as R
import qualified Data.Text                                as T
import qualified Data.UUID                                as Uuid
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC
import qualified System.Random.MWC                        as Random

import           Web.Direct.Client
import           Web.Direct.Exception
import           Web.Direct.Map
import           Web.Direct.Message
import           Web.Direct.PersistedInfo
import           Web.Direct.Types

----------------------------------------------------------------

data Config = Config {
    directCreateMessageHandler :: Client -> Message -> Aux -> IO ()
  , directLogger               :: RPC.Logger
  , directFormatter            :: RPC.Formatter
  , directEndpointUrl          :: RPC.URL -- Endpoint URL for direct WebSocket API.
  }

-- | The default configuration.
--   'RequestHandler' automatically replies ACK.
--   'NotificationHandler' and 'logger' do nothing.
--   'formatter' is 'show'.
--   'endpointUrl' is 'wss://api.direct4b.com/albero-app-server/api'
defaultConfig :: Config
defaultConfig = Config
    { directCreateMessageHandler = \_ _ _ -> return ()
    , directLogger               = \_ -> return ()
    , directFormatter            = show
    , directEndpointUrl = "wss://api.direct4b.com/albero-app-server/api"
    }

----------------------------------------------------------------

login
    :: Config
    -> T.Text -- ^ Login email address for direct.
    -> T.Text -- ^ Login password for direct.
    -> IO (Either Exception Client)
login config email pass =
    RPC.withClient (directEndpointUrl config) rpcConfig $ \rpcClient -> do
        idfv <- genIdfv

        let magicConstant = M.ObjectStr ""
            methodName    = "create_access_token"
        res <- RPC.call
            rpcClient
            methodName
            [ M.ObjectStr email
            , M.ObjectStr pass
            , M.ObjectStr idfv
            , M.ObjectStr agentName
            , magicConstant
            ]
        case resultToObjectOrException methodName res of
            Right (M.ObjectStr token) ->
                Right <$> newClient (PersistedInfo token idfv) rpcClient
            Right other -> return $ Left $ UnexpectedReponse methodName other
            Left  e     -> return $ Left e
  where
    rpcConfig = RPC.defaultConfig
        { RPC.requestHandler = \rpcClient mid _method _objs ->
             -- sending ACK always
            sendAck rpcClient mid
        , RPC.logger         = directLogger config
        , RPC.formatter      = directFormatter config
        }

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

withClient :: Config -> PersistedInfo -> (Client -> IO a) -> IO a
withClient config pInfo action = do
    ref <- I.newIORef Nothing
    RPC.withClient (directEndpointUrl config) (rpcConfig ref) $ \rpcClient -> do
        client <- newClient pInfo rpcClient
        I.writeIORef ref $ Just client
        createSession client
        subscribeNotification client
        action client
  where
    rpcConfig ref = RPC.defaultConfig
        { RPC.requestHandler = \rpcClient mid method objs -> do
            -- sending ACK always
            sendAck rpcClient mid
            Just client <- I.readIORef ref
            active      <- isActive client
            when active $ do
                -- fixme: "notify_update_domain_users"
                -- fixme: "notify_update_read_statuses"
                Just me <- getMe client
                let myid = userId me
                when (method == "notify_create_message") $ case objs of
                    M.ObjectMap rsp : _ -> case decodeMessage rsp of
                        Just (msg, aux@(Aux _ _ uid)) | uid /= myid -> do
                            echan <- findChannel client aux
                            case echan of
                                Just chan -> dispatch chan msg aux
                                Nothing   -> directCreateMessageHandler
                                    config
                                    client
                                    msg
                                    aux
                        _ -> return ()
                    _ -> return ()
        , RPC.logger         = directLogger config
        , RPC.formatter      = directFormatter config
        }

createSession :: Client -> IO ()
createSession client = do
    let methodName = "create_session"
    rsp <- callRpcThrow
        (clientRpcClient client)
        methodName
        [ M.ObjectStr $ persistedInfoDirectAccessToken $ clientPersistedInfo
            client
        , M.ObjectStr apiVersion
        , M.ObjectStr agentName
        ]

    case fromCreateSession rsp of
        Just user -> setMe client user
        _         -> E.throwIO $ UnexpectedReponse methodName rsp

subscribeNotification :: Client -> IO ()
subscribeNotification client = do
    let c = clientRpcClient client
    void $ callRpcThrow c "reset_notification" []
    void $ callRpcThrow c "start_notification" []
    doms <- callRpcThrow c "get_domains" []
    setDomains client $ fromGetDomains doms
    void $ callRpcThrow c "get_domain_invites" []
    void $ callRpcThrow c "get_account_control_requests" []
    void $ callRpcThrow c "get_joined_account_control_group" []
    void $ callRpcThrow c "get_announcement_statuses" []
    void $ callRpcThrow c "get_friends" []
    acq <- callRpcThrow c "get_acquaintances" []
    setUsers client $ fromGetAcquaintances acq
    talks <- callRpcThrow c "get_talks" []
    setTalkRooms client $ fromGetTalks talks
    void $ callRpcThrow c "get_talk_statuses" []

----------------------------------------------------------------

sendAck :: RPC.Client -> R.MessageId -> IO ()
sendAck rpcClient mid = RPC.reply rpcClient mid $ Right $ M.ObjectBool True
