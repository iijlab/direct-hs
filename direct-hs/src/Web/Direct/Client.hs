{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Direct.Client
    ( Client
    , clientRpcClient
    , clientLoginInfo
    , sendMessage
    , uploadFile
    , newClient
    , setDomains
    , getDomains
    , setTalkRooms
    , getTalkRooms
    , setMe
    , getMe
    , setAcquaintances
    , getAcquaintances
    , setUsers
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
    , shutdown
    -- re-exporting
    , dispatch
    , ChannelType
    , Channel
    , pairChannel
    , pinPointChannel
    , groupChannel
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
  , clientUsers         :: I.IORef Users
  , clientChannels      :: ChannelDB
  , clientStatus        :: StatusVar
  , clientCurrentDomain :: Domain
  }

newClient :: LoginInfo -> RPC.Client -> Domain -> User -> IO Client
newClient pinfo rpcClient initialDomain me =
    Client pinfo rpcClient
        <$> I.newIORef []
        <*> I.newIORef []
        <*> I.newIORef (Users me [])
        <*> newChannelDB
        <*> S.newTVarIO Active
        <*> pure initialDomain

----------------------------------------------------------------

setDomains :: Client -> [Domain] -> IO ()
setDomains client domains = I.writeIORef (clientDomains client) domains

getDomains :: Client -> IO [Domain]
getDomains client = I.readIORef (clientDomains client)

setTalkRooms :: Client -> [TalkRoom] -> IO ()
setTalkRooms client talks = I.writeIORef (clientTalkRooms client) talks

getTalkRooms :: Client -> IO [TalkRoom]
getTalkRooms client = I.readIORef (clientTalkRooms client)

setMe :: Client -> User -> IO ()
setMe client user = do
    users <- getUsers client
    setUsers client $ users { myself = user }

getMe :: Client -> IO User
getMe client = do
    users <- getUsers client
    return $ myself users

setAcquaintances :: Client -> [User] -> IO ()
setAcquaintances client acqs = do
    users <- getUsers client
    setUsers client $ users { acquaintances = acqs }

getAcquaintances :: Client -> IO [User]
getAcquaintances client = do
    users <- getUsers client
    return $ acquaintances users

setUsers :: Client -> Users -> IO ()
setUsers client users = I.writeIORef (clientUsers client) users

getUsers :: Client -> IO Users
getUsers client = I.readIORef (clientUsers client)

getCurrentDomain :: Client -> Domain
getCurrentDomain = clientCurrentDomain

setCurrentDomain :: Client -> Domain -> Client
setCurrentDomain client did = client { clientCurrentDomain = did }

----------------------------------------------------------------

findUser :: UserId -> Client -> IO (Maybe User)
findUser uid client = do
    users <- getUsers client
    return $ L.find (\u -> userId u == uid) (myself users : acquaintances users)

findTalkRoom :: TalkId -> Client -> IO (Maybe TalkRoom)
findTalkRoom tid client = do
    rooms <- getTalkRooms client
    return $ L.find (\r -> talkId r == tid) rooms

getTalkUsers :: Client -> TalkRoom -> IO Users
getTalkUsers client talk = do
    me    <- getMe client
    users <- catMaybes <$> mapM (`findUser` client) (talkUserIds talk)
    let talkAcqs = filter ((/= userId me) . userId) users
    return $ Users me talkAcqs

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
    user      <- failWith InvalidUserId =<< liftIO (findUser uid client)
    talkUsers <- liftIO $ getTalkUsers client talk
    when (user `notElem` acquaintances talkUsers) $ throwError InvalidUserId
    ExceptT $ deleteTalker (clientRpcClient client) talk user

----------------------------------------------------------------

-- | Sending a message in the main 'IO' or 'directCreateMessageHandler'.
sendMessage :: Client -> Message -> TalkId -> IO (Either Exception MessageId)
sendMessage client req tid = createMessage (clientRpcClient client) req tid

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
isActive client = S.atomically $ isActiveSTM $ clientStatus client

findChannel :: Client -> ChannelKey -> IO (Maybe Channel)
findChannel client ckey = findChannel' (clientChannels client) ckey

-- | A new channel is created according to the first and second arguments.
--   Then the third argument runs in a new thread with the channel.
--   In this case, 'True' is returned.
--   If 'shutdown' is already called, a new thread is not spawned
--   and 'False' is returned.
withChannel :: Client -> ChannelType -> (Channel -> IO ()) -> IO Bool
withChannel client ctyp body = withChannel' (clientRpcClient client)
                                            (clientCurrentDomain client)
                                            (clientChannels client)
                                            (clientStatus client)
                                            ctyp
                                            body

-- | This function lets 'directCreateMessageHandler' to not accept any message,
--   then sends the maintenance message to all channels,
--   and finnaly waits that all channels are closed.
shutdown :: Client -> Message -> IO ()
shutdown client msg = shutdown' (clientRpcClient client)
                                (clientChannels client)
                                (clientStatus client)
                                msg
