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
    , setUsers
    , getUsers
    , getCurrentDomain
    , setCurrentDomain
    , retrieveUsers
    , retrieveTalkRooms
    , isActive
    , findUser
    , findTalkRoom
    , findChannel
    , withChannel
    , shutdown
    -- re-exporting
    , dispatch
    , ChannelType
    , Channel
    , channelTalkRoom
    , pairChannel
    , pinPointChannel
    , groupChannel
    , send
    , recv
    )
where

import qualified Control.Concurrent.STM                   as S
import           Control.Monad.Except                     (ExceptT (ExceptT),
                                                           runExceptT)
import qualified Data.IORef                               as I
import qualified Data.List                                as L
import           Data.Maybe                               (fromMaybe)
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Client.Channel
import           Web.Direct.Client.Status
import           Web.Direct.DirectRPC                     hiding (getDomains)
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
  , clientMe            :: I.IORef (Maybe User)
  , clientUsers         :: I.IORef [User]
  , clientChannels      :: ChannelDB
  , clientStatus        :: StatusVar
  , clientCurrentDomain :: Domain
  }

newClient :: LoginInfo -> RPC.Client -> Domain -> IO Client
newClient pinfo rpcClient initialDomain =
    Client pinfo rpcClient
        <$> I.newIORef []
        <*> I.newIORef []
        <*> I.newIORef Nothing
        <*> I.newIORef []
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
setMe client user = I.writeIORef (clientMe client) (Just user)

getMe :: Client -> IO (Maybe User)
getMe client = I.readIORef (clientMe client)

setUsers :: Client -> [User] -> IO ()
setUsers client users = I.writeIORef (clientUsers client) users

-- | Getting acquaintances for me. The head of the list is myself.
getUsers :: Client -> IO [User]
getUsers client = I.readIORef (clientUsers client)

getCurrentDomain :: Client -> Domain
getCurrentDomain = clientCurrentDomain

setCurrentDomain :: Client -> Domain -> IO Client
setCurrentDomain client domain = do
    let client' = client { clientCurrentDomain = domain }
    someone <- getMe client'
    case someone of
        Just me -> do
            users <- retrieveUsers client' me
            setUsers client' users
            retrieveTalkRooms client' users >>= setTalkRooms client
        Nothing -> fail "Assertion failure: Who am I?"
    return client'

----------------------------------------------------------------

retrieveUsers :: Client -> User -> IO [User]
retrieveUsers client me = do
    acquaintances <- getAcquaintances (clientRpcClient client)
    let did   = domainId $ getCurrentDomain client
    let users = fromMaybe [] $ lookup did acquaintances
    return $ me : (me `L.delete` users)

retrieveTalkRooms :: Client -> [User] -> IO [TalkRoom]
retrieveTalkRooms client users = do
    talks <- getTalks (clientRpcClient client) users
    let did = domainId $ getCurrentDomain client
    return $ fromMaybe [] $ lookup did talks

----------------------------------------------------------------

findUser :: UserId -> Client -> IO (Maybe User)
findUser uid client = do
    users <- getUsers client
    return $ L.find (\u -> userId u == uid) users

findTalkRoom :: TalkId -> Client -> IO (Maybe TalkRoom)
findTalkRoom tid client = do
    rooms <- getTalkRooms client
    return $ L.find (\r -> talkId r == tid) rooms

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
