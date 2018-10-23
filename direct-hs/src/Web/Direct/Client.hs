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
import           Control.Error                            (hoistEither)
import           Control.Monad.Except                     (ExceptT (ExceptT),
                                                           runExceptT,
                                                           throwError)
import qualified Data.IORef                               as I
import qualified Data.List                                as L
import qualified Data.MessagePack                         as M
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
    clientLoginInfo :: !LoginInfo
  , clientRpcClient :: !RPC.Client
  , clientDomains   :: I.IORef [Domain]
  , clientTalkRooms :: I.IORef [TalkRoom]
  , clientMe        :: I.IORef (Maybe User)
  , clientUsers     :: I.IORef [User]
  , clientChannels  :: ChannelDB
  , clientStatus    :: StatusVar
  }

newClient :: LoginInfo -> RPC.Client -> IO Client
newClient pinfo rpcClient =
    Client pinfo rpcClient
        <$> I.newIORef []
        <*> I.newIORef []
        <*> I.newIORef Nothing
        <*> I.newIORef []
        <*> newChannelDB
        <*> S.newTVarIO Active

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

uploadFile :: Client -> UploadFile -> Aux -> IO (Either Exception MessageId)
uploadFile client upf@UploadFile {..} aux = runExceptT $ do
    let methodName = "create_upload_auth"
        obj        = toCreateUploadAuth upf
    rsp <- ExceptT
        (   resultToObjectOrException methodName
        <$> RPC.call (clientRpcClient client) methodName obj
        )
    case rsp of
        M.ObjectMap rspMap -> do
            ua@UploadAuth {..} <- hoistEither
                $ decodeUploadAuth methodName rsp rspMap
            ExceptT $ runUploadFile upf ua
            let files = Files
                    [ File uploadAuthGetUrl
                           uploadFileMimeType
                           uploadFileSize
                           uploadFileName
                           uploadAuthFileId
                    ]
                    uploadFileAttachedText
            ExceptT $ sendMessage client files aux
        other -> throwError $ UnexpectedReponse methodName other

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
