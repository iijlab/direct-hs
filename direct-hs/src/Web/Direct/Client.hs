{-# LANGUAGE RecordWildCards #-}

module Web.Direct.Client
    ( Client
    , clientRpcClient
    , clientLoginInfo
    , clientChannels
    , sendMessage
    , uploadFile
    , newClient
    , setDomains
    , getDomains
    , modifyTalkRooms
    , setTalkRooms
    , getTalkRooms
    , setMe
    , getMe
    , setAcquaintances
    , getAcquaintances
    , modifyAcquaintances
    , getUsers
    , getCurrentDomain
    , setCurrentDomain
    , isActive
    , findUser
    , findTalkRoom
    , getTalkUsers
    , leaveTalkRoom
    , removeUserFromTalkRoom
    , findChannel
    , withChannel
    , getChannelAcquaintances
    , shutdown
    -- re-exporting
    , dispatch
    , Channel
    , haltChannel
    , getChannels
    , send
    , recv
    )
where

import qualified Control.Concurrent.STM                   as S
import           Control.Error.Util                       (failWith)
import           Control.Monad                            (when)
import           Control.Monad.Except                     (ExceptT (ExceptT),
                                                           runExceptT,
                                                           throwError)
import           Control.Monad.IO.Class                   (liftIO)
import qualified Data.IORef                               as I
import qualified Data.List                                as L
import           Data.Maybe                               (catMaybes)
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Client.Channel
import           Web.Direct.Client.Status
import           Web.Direct.DirectRPC                     hiding
                                                           (getAcquaintances,
                                                           getDomains)
import           Web.Direct.Exception
import           Web.Direct.LoginInfo
import           Web.Direct.Message
import           Web.Direct.Types
import           Web.Direct.Upload

----------------------------------------------------------------

-- | Direct client.
data Client = Client {
    clientLoginInfo     :: !LoginInfo
  , clientRpcClient     :: !RPC.Client
  , clientDomains       :: I.IORef [Domain]
  , clientTalkRooms     :: I.IORef [TalkRoom]
  , clientMe            :: I.IORef User
  , clientAcquaintances :: I.IORef [User]
  , clientChannels      :: ChannelDB
  , clientStatus        :: StatusVar
  , clientCurrentDomain :: Domain
  }

newClient :: LoginInfo -> RPC.Client -> Domain -> User -> IO Client
newClient pinfo rpcClient initialDomain me =
    Client pinfo rpcClient
        <$> I.newIORef []
        <*> I.newIORef []
        <*> I.newIORef me
        <*> I.newIORef []
        <*> newChannelDB
        <*> S.newTVarIO Active
        <*> pure initialDomain

----------------------------------------------------------------

setDomains :: Client -> [Domain] -> IO ()
setDomains = I.writeIORef . clientDomains

getDomains :: Client -> IO [Domain]
getDomains client = I.readIORef (clientDomains client)

modifyTalkRooms :: Client -> ([TalkRoom] -> ([TalkRoom], r)) -> IO r
modifyTalkRooms client = I.atomicModifyIORef' (clientTalkRooms client)

setTalkRooms :: Client -> [TalkRoom] -> IO ()
setTalkRooms = I.writeIORef . clientTalkRooms

getTalkRooms :: Client -> IO [TalkRoom]
getTalkRooms = I.readIORef . clientTalkRooms

setMe :: Client -> User -> IO ()
setMe = I.writeIORef . clientMe

getMe :: Client -> IO User
getMe = I.readIORef . clientMe

setAcquaintances :: Client -> [User] -> IO ()
setAcquaintances = I.writeIORef . clientAcquaintances

getAcquaintances :: Client -> IO [User]
getAcquaintances = I.readIORef . clientAcquaintances

modifyAcquaintances :: Client -> ([User] -> ([User], r)) -> IO r
modifyAcquaintances client = I.atomicModifyIORef' (clientAcquaintances client)

--- | Getting acquaintances and me. The head of the list is myself.
getUsers :: Client -> IO [User]
getUsers client = do
    me   <- getMe client
    acqs <- getAcquaintances client
    return $ me : acqs

getCurrentDomain :: Client -> Domain
getCurrentDomain = clientCurrentDomain

setCurrentDomain :: Client -> Domain -> Client
setCurrentDomain client did = client { clientCurrentDomain = did }

----------------------------------------------------------------

findUser :: UserId -> Client -> IO (Maybe User)
findUser uid client = do
    users <- getUsers client
    return $ L.find (\u -> userId u == uid) users

findTalkRoom :: TalkId -> Client -> IO (Maybe TalkRoom)
findTalkRoom tid client = do
    rooms <- getTalkRooms client
    return $ L.find (\r -> talkId r == tid) rooms

--- | Getting talk room members. The head of the list is myself.
getTalkUsers :: Client -> TalkRoom -> IO [User]
getTalkUsers client talk = do
    me       <- getMe client
    talkAcqs <- getTalkAcquaintances client talk
    return $ me : talkAcqs

getTalkAcquaintances :: Client -> TalkRoom -> IO [User]
getTalkAcquaintances client talk = do
    me    <- getMe client
    users <- catMaybes <$> mapM (`findUser` client) (talkUserIds talk)
    return $ filter ((/= userId me) . userId) users

----------------------------------------------------------------

leaveTalkRoom :: Client -> TalkId -> IO (Either Exception ())
leaveTalkRoom client tid = runExceptT $ do
    talk <- failWith InvalidTalkId =<< liftIO (findTalkRoom tid client)
    me   <- liftIO $ getMe client
    ExceptT $ deleteTalker (clientRpcClient client) talk me

removeUserFromTalkRoom :: Client -> TalkId -> UserId -> IO (Either Exception ())
removeUserFromTalkRoom client tid uid = runExceptT $ do
    talk <- failWith InvalidTalkId =<< liftIO (findTalkRoom tid client)
    -- Can not ban a friend on PairTalk
    when (talkType talk == PairTalk) $ throwError InvalidTalkType
    user     <- failWith InvalidUserId =<< liftIO (findUser uid client)
    talkAcqs <- liftIO $ getTalkAcquaintances client talk
    when (user `notElem` talkAcqs) $ throwError InvalidUserId
    ExceptT $ deleteTalker (clientRpcClient client) talk user

----------------------------------------------------------------

-- | Sending a message in the main 'IO' or 'directCreateMessageHandler'.
sendMessage :: Client -> Message -> TalkId -> IO (Either Exception MessageId)
sendMessage = createMessage . clientRpcClient

----------------------------------------------------------------

uploadFile :: Client -> UploadFile -> TalkId -> IO (Either Exception MessageId)
uploadFile client upf@UploadFile {..} tid = runExceptT $ do
    ua@UploadAuth {..} <- ExceptT $ createUploadAuth
        (clientRpcClient client)
        uploadFileName
        uploadFileMimeType
        uploadFileSize
        (getCurrentDomain client)
    ExceptT $ runUploadFile upf ua
    let files = Files
            [ File uploadAuthGetUrl
                   uploadFileMimeType
                   uploadFileSize
                   uploadFileName
                   uploadAuthFileId
            ]
            uploadFileAttachedText
    ExceptT $ sendMessage client files tid

isActive :: Client -> IO Bool
isActive = S.atomically . isActiveSTM . clientStatus

findChannel :: Client -> ChannelKey -> IO (Maybe Channel)
findChannel = findChannel' . clientChannels

-- | A new channel is created according to the first three arguments.
--   Then the fourth argument runs in a new thread with the channel.
--   In this case, 'True' is returned.
--   If 'shutdown' is already called, a new thread is not spawned
--   and 'False' is returned.
withChannel
    :: Client
    -> TalkRoom -- ^ where to talk
    -> Maybe User -- ^ limit of who to talk with; 'Nothing' means everyone (no limits)
    -> (Channel -> IO ())
    -> IO Bool
withChannel client = withChannel'
    (clientRpcClient client)
    (clientChannels client)
    (clientStatus client)

getChannelAcquaintances :: Client -> Channel -> IO [User]
getChannelAcquaintances client chan = case channelUserLimit chan of
    Just user -> return [user]
    Nothing   -> getTalkAcquaintances client $ channelTalkRoom chan

-- | This function lets 'directCreateMessageHandler' to not accept any message,
--   then sends the maintenance message to all channels,
--   and finnaly waits that all channels are closed.
shutdown :: Client -> Message -> IO ()
shutdown client = shutdown' (clientRpcClient client)
                            (clientChannels client)
                            (clientStatus client)
