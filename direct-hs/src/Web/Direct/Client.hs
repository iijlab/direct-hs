{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Client
    ( Client
    , clientRpcClient
    , clientLoginInfo
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
    , Channel
    , withChannel
    , channelTalkRoom
    , findChannel
    , dispatch
    , shutdown
    , sendMessage
    , send
    , recv
    , findUser
    , findTalkRoom
    , findPairTalkRoom
    , ChannelType
    , pairChannel
    , pinPointChannel
    , groupChannel
    )
where

import qualified Control.Concurrent                       as C
import qualified Control.Concurrent.STM                   as S
import           Control.Monad                            (void)
import qualified Data.IORef                               as I
import qualified Data.List                                as L
import qualified Data.Map.Strict                          as HM
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Channel
import           Web.Direct.DirectRPC hiding (getDomains)
import           Web.Direct.Exception
import           Web.Direct.LoginInfo
import           Web.Direct.Message
import           Web.Direct.Types

----------------------------------------------------------------

data Status = Active | Inactive deriving Eq

----------------------------------------------------------------

-- | Direct client.
data Client = Client {
    clientLoginInfo :: !LoginInfo
  , clientRpcClient :: !RPC.Client
  , clientDomains   :: I.IORef [Domain]
  , clientTalkRooms :: I.IORef [TalkRoom]
  , clientMe        :: I.IORef (Maybe User)
  , clientUsers     :: I.IORef [User]
  , clientChannels  :: S.TVar (HM.Map ChannelKey Channel)
  , clientStatus    :: S.TVar Status
  }

newClient :: LoginInfo -> RPC.Client -> IO Client
newClient pinfo rpcClient =
    Client pinfo rpcClient
        <$> I.newIORef []
        <*> I.newIORef []
        <*> I.newIORef Nothing
        <*> I.newIORef []
        <*> S.newTVarIO HM.empty
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

isActive :: Client -> IO Bool
isActive client = S.atomically $ isActiveSTM client

isActiveSTM :: Client -> S.STM Bool
isActiveSTM client = (== Active) <$> S.readTVar (clientStatus client)

inactivate :: Client -> IO ()
inactivate client = S.atomically $ S.writeTVar (clientStatus client) Inactive

----------------------------------------------------------------

-- | Creating a new channel.
--   This returns 'Nothing' after 'shutdown'.
allocateChannel :: Client -> ChannelType -> IO (Maybe Channel)
allocateChannel client ctyp = do
    (room, muser) <- case ctyp of
      Pair user -> do
          room <- createPairTalk (clientRpcClient client) user
          return (room, Just user)
      PinPoint room user -> return (room, Just user)
      Group room         -> return (room, Nothing)
    let ckey = (talkId room, userId <$> muser)
    chan <- newChannel (clientRpcClient client) ctyp ckey room
    S.atomically $ do
        active <- isActiveSTM client
        if active
            then do
                S.modifyTVar' chanDB $ HM.insert ckey chan
                return $ Just chan
            else return Nothing
    where chanDB = clientChannels client

freeChannel :: Client -> Channel -> IO ()
freeChannel client chan = S.atomically $ S.modifyTVar' chanDB $ HM.delete key
  where
    chanDB = clientChannels client
    key    = channelKey chan

allChannels :: Client -> IO [Channel]
allChannels client = HM.elems <$> S.atomically (S.readTVar chanDB)
    where chanDB = clientChannels client

findChannel :: Client -> ChannelKey -> IO (Maybe Channel)
findChannel client key = HM.lookup key <$> S.atomically (S.readTVar chanDB)
  where
    chanDB = clientChannels client

----------------------------------------------------------------

wait :: Client -> IO ()
wait client = S.atomically $ do
    db <- S.readTVar chanDB
    S.check $ HM.null db
    where chanDB = clientChannels client

----------------------------------------------------------------

-- | Sending a message in the main 'IO' or 'directCreateMessageHandler'.
sendMessage :: Client -> Message -> TalkId -> IO (Either Exception MessageId)
sendMessage client req tid = createMessage (clientRpcClient client) req tid

----------------------------------------------------------------

-- | A new channel is created according to the first and second arguments.
--   Then the third argument runs in a new thread with the channel.
--   In this case, 'True' is returned.
--   If 'shutdown' is already called, a new thread is not spawned
--   and 'False' is returned.
withChannel :: Client -> ChannelType -> (Channel -> IO ()) -> IO Bool
withChannel client ctyp body = do
    mchan <- allocateChannel client ctyp
    case mchan of
        Nothing   -> return False
        Just chan -> do
            void $ C.forkFinally (body chan) $ \e -> do
                print e
                freeChannel client chan
            return True

-- | This function lets 'directCreateMessageHandler' to not accept any message,
--   then sends the maintenance message to all channels,
--   and finnaly waits that all channels are closed.
shutdown :: Client -> Message -> IO ()
shutdown client msg = do
    inactivate client
    chans <- allChannels client
    mapM_ (die msg) chans
    wait client
    RPC.shutdown $ clientRpcClient client
