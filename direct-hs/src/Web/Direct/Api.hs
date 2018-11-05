{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Api
    ( Config(..)
    , defaultConfig
    , login
    , RPC.URL
    , withClient
    )
where

import           Control.Monad                            (when)
import qualified Data.IORef                               as I
import qualified Data.List                                as L
import qualified Data.MessagePack                         as M
import qualified Data.MessagePack.RPC                     as R
import qualified Data.Text                                as T
import qualified Data.UUID                                as Uuid
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC
import qualified System.Random.MWC                        as Random

import           Web.Direct.Client                        hiding (getDomains)
import           Web.Direct.DirectRPC
import           Web.Direct.Exception
import           Web.Direct.LoginInfo
import           Web.Direct.Message
import           Web.Direct.Types

----------------------------------------------------------------

-- | Type for client configuration.
data Config = Config {
    directCreateMessageHandler     :: Client -> (Message, MessageId, TalkRoom, User) -> IO ()
    -- ^ Called every time receiving a new message from the server.
  , directLogger                   :: RPC.Logger
  , directFormatter                :: RPC.Formatter
  , directEndpointUrl              :: RPC.URL
    -- ^ Endpoint URL for direct WebSocket API.
  , directWaitCreateMessageHandler :: Bool
    -- ^ If @True@, 'withClient' function doesen't finish until the
    --   'directCreateMessageHandler' thread finish.
    --   Disable this to write a batch application, which just send a message
    --   once or more, then finishes.
    --   Default: @True@.
  , directInitialDomainId          :: Maybe DomainId
    -- ^ Domain ID used with some RPC functions which requires a domain ID for its argument (e.g. @createPairTalk@, @createUploadAuth@).
    --   If @Nothing@, the first domain obtained by @get_domains@ RPC function is used.
    --   If you want to change the target domain in the 'withClient' block,
    --   Use 'setCurrentDomainId' for 'Client'.
  }

-- | The default configuration.
--
--   * 'directCreateMessageHandler' and 'directLogger' do nothing.
--   * 'directFormatter' is 'show'.
--   * 'directEndpointUrl' is 'wss://api.direct4b.com/albero-app-server/api'
defaultConfig :: Config
defaultConfig = Config
    { directCreateMessageHandler     = \_ _ -> return ()
    , directLogger                   = \_ -> return ()
    , directFormatter                = show
    , directEndpointUrl = "wss://api.direct4b.com/albero-app-server/api"
    , directWaitCreateMessageHandler = True
    , directInitialDomainId          = Nothing
    }

----------------------------------------------------------------

-- | Logging in the Direct cloud.
login
    :: Config -- ^ The configuration. NOTE: 'directCreateMessageHandler' and 'directWaitCreateMessageHandler' are ignored.
    -> T.Text -- ^ Login email address for direct.
    -> T.Text -- ^ Login password for direct.
    -> IO (Either Exception LoginInfo) -- ^ This should be passed to 'withClient'.
login config email pass =
    RPC.withClient (directEndpointUrl config) rpcConfig $ \rpcClient -> do
        idfv <- genIdfv
        createAccessToken rpcClient email pass idfv agentName
  where
    rpcConfig = RPC.defaultConfig
        { RPC.requestHandler     = \rpcClient mid _method _objs ->
             -- sending ACK always
                                       sendAck rpcClient mid
        , RPC.logger             = directLogger config
        , RPC.formatter          = directFormatter config
        , RPC.waitRequestHandler = False
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

----------------------------------------------------------------

withClient :: Config -> LoginInfo -> (Client -> IO a) -> IO a
withClient config pInfo action = do
    ref <- I.newIORef Nothing
    RPC.withClient (directEndpointUrl config) (rpcConfig ref) $ \rpcClient -> do
        me <- createSession rpcClient (loginInfoDirectAccessToken pInfo)
        initialDomain <- decideInitialDomain config rpcClient
        client <- newClient pInfo rpcClient initialDomain
        I.writeIORef ref $ Just client
        setMe client me
        subscribeNotification client me
        action client
  where
    rpcConfig ref = RPC.defaultConfig
        { RPC.requestHandler     =
            \rpcClient mid method objs -> do
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
                            Just (msg, msgid, tid, uid)
                                | uid /= myid && uid /= 0 -> do
                                    mchan <- findChannel client (tid, Just uid)
                                    Just user <- findUser uid client
                                    Just room <- findTalkRoom tid client
                                    case mchan of
                                        Just chan ->
                                            dispatch chan msg msgid room user
                                        Nothing -> do
                                            mchan' <- findChannel
                                                client
                                                (tid, Nothing)
                                            case mchan' of
                                                Just chan' -> dispatch
                                                    chan'
                                                    msg
                                                    msgid
                                                    room
                                                    user
                                                Nothing ->
                                                    directCreateMessageHandler
                                                        config
                                                        client
                                                        (msg, msgid, room, user)
                            _ -> return ()
                        _ -> return ()
        , RPC.logger             = directLogger config
        , RPC.formatter          = directFormatter config
        , RPC.waitRequestHandler = directWaitCreateMessageHandler config
        }

decideInitialDomain :: Config -> RPC.Client -> IO Domain
decideInitialDomain config rpcclient = do
    doms <- getDomains rpcclient
    case directInitialDomainId config of
        Just did -> case L.find (\dom -> domainId dom == did) doms of
            Just dom -> return dom
            -- TODO: This exception is obviously recoverable by the library user.
            --       Return a Left exception?
            _        -> fail $ "ERROR: You don't belong to domain#" ++ show did
        _ -> case doms of
            []        -> fail "Assertion failure: no domains obtained!"
            (dom : _) -> return dom

subscribeNotification :: Client -> User -> IO ()
subscribeNotification client me = do
    let rpcclient = clientRpcClient client
    resetNotification rpcclient
    startNotification rpcclient
    getDomains rpcclient >>= setDomains client
    getDomainInvites rpcclient
    getAccountControlRequests rpcclient
    getJoinedAccountControlGroup rpcclient
    getAnnouncementStatuses rpcclient
    getFriends rpcclient

    users <- retrieveUsers client me
    setUsers client users
    retrieveTalkRooms client users >>= setTalkRooms client
    getTalkStatuses rpcclient

----------------------------------------------------------------

sendAck :: RPC.Client -> R.MessageId -> IO ()
sendAck rpcClient mid = RPC.reply rpcClient mid $ Right $ M.ObjectBool True
