module Web.Direct.Api
    ( Config(..)
    , defaultConfig
    , defaultNotificationHandlers
    , login
    , RPC.URL
    , withClient
    )
where

import           Control.Monad                            (when)
import qualified Data.IORef                               as I
import qualified Data.List                                as L
import           Data.Maybe                               (fromMaybe)
import qualified Data.MessagePack                         as M
import qualified Data.MessagePack.RPC                     as R
import qualified Data.Text                                as T
import qualified Data.UUID                                as Uuid
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC
import qualified System.Random.MWC                        as Random

import           Web.Direct.Client                        hiding
                                                           (getAcquaintances,
                                                           getDomains)
import           Web.Direct.DirectRPC
import           Web.Direct.Exception
import           Web.Direct.LoginInfo
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
  , directNotificationHandlers     :: NotificationHandlers
    -- ^ Called every time after receiving a notification.
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
    , directNotificationHandlers     = defaultNotificationHandlers
    }

-- | The default notification handlers. Do nothing by default.
defaultNotificationHandlers :: NotificationHandlers
defaultNotificationHandlers = NotificationHandlers
    { onNotifyCreateMessage      = \_ _ _ _ -> return ()
    , onNotifyAddTalkers         = \_ _ -> return ()
    , onNotifyAddAcquaintance    = \_ _ -> return ()
    , onNotifyDeleteTalk         = \_ -> return ()
    , onNotifyDeleteTalker       = \_ _ _ _ -> return ()
    , onNotifyDeleteAcquaintance = \_ _ -> return ()
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
        client <- newClient pInfo rpcClient initialDomain me
        I.writeIORef ref $ Just client
        subscribeNotification client
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
                    let userNH   = directNotificationHandlers config
                        handlers = NotificationHandlers
                            { onNotifyCreateMessage = \msg msdId tid uid -> do
                                handleNotifyCreateMessage config
                                                          client
                                                          msg
                                                          msdId
                                                          tid
                                                          uid
                                onNotifyCreateMessage userNH msg msdId tid uid
                            , onNotifyAddTalkers = \did room -> do
                                onAddTalkers client did room
                                onNotifyAddTalkers userNH did room
                            , onNotifyAddAcquaintance = \did user -> do
                                handleAddAcquaintance client did user
                                onNotifyAddAcquaintance userNH did user
                            , onNotifyDeleteTalk = \tid -> do
                                onDeleteTalk client tid
                                onNotifyDeleteTalk userNH tid
                            , onNotifyDeleteTalker = \did tid uids leftUids ->
                                do
                                    onDeleteTalker client did tid uids leftUids
                                    onNotifyDeleteTalker userNH
                                                         did
                                                         tid
                                                         uids
                                                         leftUids
                            , onNotifyDeleteAcquaintance = \did uid -> do
                                handleNotifyDeleteAcquaintance client did uid
                                onNotifyDeleteAcquaintance userNH did uid
                            }
                    handleNotification method objs handlers
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

subscribeNotification :: Client -> IO ()
subscribeNotification client = do
    let rpcclient = clientRpcClient client
    resetNotification rpcclient
    startNotification rpcclient
    getDomains rpcclient >>= setDomains client
    getDomainInvites rpcclient
    getAccountControlRequests rpcclient
    getJoinedAccountControlGroup rpcclient
    getAnnouncementStatuses rpcclient
    getFriends rpcclient

    let did = domainId $ getCurrentDomain client
    _ <- initialiseAcquaintances client
    allTalks <- getTalks rpcclient
    let talks = fromMaybe [] $ lookup did allTalks
    setTalkRooms client talks
    getTalkStatuses rpcclient

----------------------------------------------------------------

sendAck :: RPC.Client -> R.MessageId -> IO ()
sendAck rpcClient mid = RPC.reply rpcClient mid $ Right $ M.ObjectBool True

----------------------------------------------------------------

handleNotifyCreateMessage
    :: Config -> Client -> Message -> MessageId -> TalkId -> UserId -> IO ()
handleNotifyCreateMessage config client msg msgid tid uid = do
    me <- getMe client
    let myid = userId me
    when (uid /= myid && uid /= 0) $ do
        mchan     <- findChannel client (tid, Just uid)
        Just user <- findUser uid client
        Just room <- findTalkRoom tid client
        case mchan of
            Just chan -> dispatch chan msg msgid room user
            Nothing   -> do
                mchan' <- findChannel client (tid, Nothing)
                case mchan' of
                    Just chan' -> dispatch chan' msg msgid room user
                    Nothing    -> directCreateMessageHandler
                        config
                        client
                        (msg, msgid, room, user)

handleAddAcquaintance :: Client -> DomainId -> User -> IO ()
handleAddAcquaintance client _did newUser =
    modifyAcquaintances client $ \users -> (newUser : users, ())

handleNotifyDeleteAcquaintance :: Client -> DomainId -> UserId -> IO ()
handleNotifyDeleteAcquaintance client _did uid = modifyAcquaintances
    client
    (\users -> (filter ((/= uid) . userId) users, ()))
