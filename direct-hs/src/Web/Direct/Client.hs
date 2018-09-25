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
    , findChannel
    , dispatch
    , shutdown
    , sendMessage
    , send
    , recv
    , findUser
    , findTalkRoom
    , findPairTalkRoom
    , ChannelType(..)
    , pairChannel
    , groupChannel
    )
where

import qualified Control.Concurrent                       as C
import qualified Control.Concurrent.STM                   as S
import qualified Control.Exception                        as E
import           Control.Monad                            (void)
import qualified Data.Map.Strict                          as HM
import qualified Data.IORef                               as I
import qualified Data.List                                as L
import qualified Data.MessagePack                         as M
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Exception
import           Web.Direct.LoginInfo
import           Web.Direct.Message
import           Web.Direct.Types

----------------------------------------------------------------

data Status = Active | Inactive deriving Eq

data ChannelType = Pair !TalkId !UserId
                 | Group !TalkId
                 deriving (Eq, Ord, Show)

channelTalkId :: ChannelType -> TalkId
channelTalkId (Pair tid _) = tid
channelTalkId (Group tid)  = tid

groupChannel :: TalkRoom -> ChannelType
groupChannel room = Group (talkId room)

pairChannel :: TalkRoom -> User -> ChannelType
pairChannel room user = Pair (talkId room) (userId user)

type ChannelKey = ChannelType

-- | A virtual communication channel based on
--   a pair of talk room ID and user ID.
data Channel = Channel {
      toWorker      :: C.MVar (Either Control (Message, MessageId))
    , channelClient :: Client
    , channelType   :: ChannelType
    }

newtype Control = Die Message

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
        (\room -> talkType room == PairTalk && uid `elem` (map userId $ talkUsers room))
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
newChannel :: Client -> ChannelType -> IO (Maybe Channel)
newChannel client ctyp = do
    mvar <- C.newEmptyMVar
    let key = ctyp
    let chan =
            Channel {toWorker = mvar, channelClient = client, channelType = ctyp}
    S.atomically $ do
        active <- isActiveSTM client
        if active
            then do
                S.modifyTVar' chanDB $ HM.insert key chan
                return $ Just chan
            else return Nothing
    where chanDB = clientChannels client

freeChannel :: Client -> ChannelType -> IO ()
freeChannel client ctyp = S.atomically $ S.modifyTVar' chanDB $ HM.delete key
  where
    chanDB = clientChannels client
    key    = ctyp

allChannels :: Client -> IO [Channel]
allChannels client = HM.elems <$> S.atomically (S.readTVar chanDB)
    where chanDB = clientChannels client

findChannel :: Client -> ChannelType -> IO (Maybe Channel)
findChannel client ctyp = HM.lookup key <$> S.atomically (S.readTVar chanDB)
  where
    chanDB = clientChannels client
    key    = ctyp

----------------------------------------------------------------

dispatch :: Channel -> Message -> MessageId -> IO ()
dispatch chan msg mid = C.putMVar (toWorker chan) $ Right (msg, mid)

control :: Channel -> Control -> IO ()
control chan ctl = C.putMVar (toWorker chan) $ Left ctl

wait :: Client -> IO ()
wait client = S.atomically $ do
    db <- S.readTVar chanDB
    S.check $ HM.null db
    where chanDB = clientChannels client

----------------------------------------------------------------

-- | Sending a message in the main 'IO' or 'directCreateMessageHandler'.
sendMessage :: Client -> Message -> TalkId -> IO (Either Exception MessageId)
sendMessage client req tid = do
    let obj        = encodeMessage req tid
        methodName = "create_message"
    ersp <-
        resultToObjectOrException methodName
            <$> RPC.call (clientRpcClient client) methodName obj
    case ersp of
        Right rsp@(M.ObjectMap rspMap) ->
            case lookup (M.ObjectStr "message_id") rspMap of
                Just (M.ObjectWord x) -> return $ Right x
                _ -> return $ Left $ UnexpectedReponse methodName rsp
        Right other -> return $ Left $ UnexpectedReponse methodName other
        Left  other -> return $ Left other

----------------------------------------------------------------

-- | A new channel is created according to the first and second arguments.
--   Then the third argument runs in a new thread with the channel.
--   In this case, 'True' is returned.
--   If 'shutdown' is already called, a new thread is not spawned
--   and 'False' is returned.
withChannel :: Client -> ChannelType -> (Channel -> IO ()) -> IO Bool
withChannel client ctyp body = do
    mchan <- newChannel client ctyp
    case mchan of
        Nothing   -> return False
        Just chan -> do
            void $ C.forkFinally (body chan) $ \_ -> freeChannel client ctyp
            return True

-- | Receiving a message from the channel.
recv :: Channel -> IO (Message, MessageId)
recv chan = do
    cm <- C.takeMVar $ toWorker chan
    case cm of
        Right msg            -> return msg
        Left  (Die announce) -> do
            let tid = channelTalkId $ channelType chan
            void $ sendMessage (channelClient chan) announce tid
            E.throwIO E.ThreadKilled

-- | Sending a message to the channel.
send :: Channel -> Message -> IO (Either Exception MessageId)
send chan msg = sendMessage (channelClient chan) msg tid
  where
    tid = channelTalkId $ channelType chan

-- | This function lets 'directCreateMessageHandler' to not accept any message,
--   then sends the maintenance message to all channels,
--   and finnaly waits that all channels are closed.
shutdown :: Client -> Message -> IO ()
shutdown client msg = do
    inactivate client
    chans <- allChannels client
    mapM_ (\chan -> control chan (Die msg)) chans
    wait client
    RPC.shutdown $ clientRpcClient client
