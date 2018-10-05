{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Client
    ( Client
    , clientRpcClient
    , clientLoginInfo
    , sendMessage
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
    , findPairTalkRoom
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
import qualified Data.IORef                               as I
import qualified Data.List                                as L
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Client.Channel
import           Web.Direct.Client.Status
import           Web.Direct.DirectRPC hiding (getDomains)
import           Web.Direct.Exception
import           Web.Direct.LoginInfo
import           Web.Direct.Message
import           Web.Direct.Types

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

findPairTalkRoom :: UserId -> Client -> IO (Maybe TalkRoom)
findPairTalkRoom uid client = do
    rooms <- getTalkRooms client
    return $ L.find
        (\room ->
            talkType room
                ==     PairTalk
                &&     uid
                `elem` (map userId $ talkUsers room)
        )
        rooms

----------------------------------------------------------------

-- | Sending a message in the main 'IO' or 'directCreateMessageHandler'.
sendMessage :: Client -> Message -> TalkId -> IO (Either Exception MessageId)
sendMessage client req tid = createMessage (clientRpcClient client) req tid

isActive :: Client -> IO Bool
isActive client = S.atomically $ isActiveSTM $ clientStatus client

findChannel :: Client -> ChannelKey -> IO (Maybe Channel)
findChannel client ckey = findChannel' (clientChannels client) ckey

withChannel :: Client -> ChannelType -> (Channel -> IO ()) -> IO Bool
withChannel client ctyp body = withChannel' (clientRpcClient client) (clientChannels client) (clientStatus client) ctyp body

shutdown :: Client -> Message -> IO ()
shutdown client msg = shutdown' (clientRpcClient client) (clientChannels client) (clientStatus client) msg
