{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Client (
    Client
  , clientRpcClient
  , clientPersistedInfo
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
  , inactivate
  , Channel
  , withChannel
  , newChannel
  , freeChannel
  , allChannels
  , findChannel
  , dispatch
  , shutdown
  , sendMessage
  , send
  , recv
  ) where

import qualified Control.Concurrent                       as C
import qualified Control.Exception                        as E
import           Control.Monad                            (void)
import qualified Data.HashMap.Strict                      as HM
import qualified Data.IORef                               as I
import qualified Data.MessagePack                         as M
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Message
import           Web.Direct.PersistedInfo
import           Web.Direct.Types

----------------------------------------------------------------

data Status = Active | Inactive deriving Eq

type ChannelKey = (TalkId, UserId)

data Channel = Channel {
      toWorker      :: C.MVar (Either Control (Message, Aux))
    , fromWorker    :: C.MVar ()
    , channelClient :: Client
    , channelAux    :: Aux
    }

newtype Control = Die Message

----------------------------------------------------------------

-- | Direct client.
data Client = Client {
    clientPersistedInfo :: !PersistedInfo
  , clientRpcClient     :: !RPC.Client
  , clientDomains       :: I.IORef [Domain]
  , clientTalkRooms     :: I.IORef [TalkRoom]
  , clientMe            :: I.IORef (Maybe User)
  , clientUsers         :: I.IORef [User]
  , clientChannels      :: I.IORef (HM.HashMap ChannelKey Channel)
  , clientStatus        :: I.IORef Status
  }

newClient :: PersistedInfo -> RPC.Client -> IO Client
newClient pinfo rpcClient =
    Client pinfo rpcClient
        <$> I.newIORef []
        <*> I.newIORef []
        <*> I.newIORef Nothing
        <*> I.newIORef []
        <*> I.newIORef HM.empty
        <*> I.newIORef Active

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

isActive :: Client -> IO Bool
isActive client = do
    status <- I.readIORef $ clientStatus client
    return $ status == Active

inactivate :: Client -> IO ()
inactivate client = I.writeIORef (clientStatus client) Inactive

----------------------------------------------------------------

fromAux :: Aux -> ChannelKey
fromAux (Aux tid _ uid) = (tid, uid)

newChannel :: Client -> Aux -> IO Channel
newChannel client aux = do
    mvar1 <- C.newEmptyMVar
    mvar2 <- C.newEmptyMVar
    let chan = Channel
            { toWorker      = mvar1
            , fromWorker    = mvar2
            , channelClient = client
            , channelAux    = aux
            }
    I.atomicModifyIORef' ref $ \m -> (HM.insert key chan m, ())
    return chan
  where
    ref = clientChannels client
    key = fromAux aux

freeChannel :: Client -> Aux -> IO ()
freeChannel client aux = I.atomicModifyIORef' ref $ \m -> (HM.delete key m, ())
  where
    ref = clientChannels client
    key = fromAux aux

allChannels :: Client -> IO [Channel]
allChannels client = HM.elems <$> I.readIORef ref
    where ref = clientChannels client

findChannel :: Client -> Aux -> IO (Maybe Channel)
findChannel client aux = HM.lookup key <$> I.readIORef ref
  where
    ref = clientChannels client
    key = fromAux aux

----------------------------------------------------------------

dispatch :: Channel -> Message -> Aux -> IO ()
dispatch chan msg aux = C.putMVar (toWorker chan) $ Right (msg, aux)

control :: Channel -> Control -> IO ()
control chan ctl = C.putMVar (toWorker chan) $ Left ctl

wait :: Channel -> IO ()
wait chan = C.takeMVar (fromWorker chan)

----------------------------------------------------------------

sendMessage :: Client -> Message -> Aux -> IO MessageId
sendMessage client req aux = do
    let obj = encodeMessage req aux
    ersp <- RPC.call (clientRpcClient client) "create_message" obj
    case ersp of
        Right (M.ObjectMap rsp) ->
            case lookup (M.ObjectStr "message_id") rsp of
                Just (M.ObjectWord x) -> return x
                _                     -> error "sendMessage" -- fixme
        _ -> error "sendMessage" -- fixme

----------------------------------------------------------------

-- | A new channel is created according to the first and second arguments.
--   Then the third argument runs in a new thread with the channel.
withChannel :: Client -> Aux -> (Channel -> IO ()) -> IO ()
withChannel client aux body = do
    chan <- newChannel client aux
    void $ C.forkFinally (body chan) $ \_ -> do
        freeChannel client            aux
        C.putMVar   (fromWorker chan) ()

recv :: Channel -> IO (Message, Aux)
recv chan = do
    cm <- C.takeMVar $ (toWorker chan)
    case cm of
        Right msg            -> return msg
        Left  (Die announce) -> do
            void $ sendMessage (channelClient chan) announce (channelAux chan)
            E.throwIO E.ThreadKilled

send :: Channel -> Message -> IO MessageId
send chan msg = sendMessage (channelClient chan) msg (channelAux chan)

shutdown :: Client -> Message -> IO ()
shutdown client msg = do
    inactivate client
    chans <- allChannels client
    mapM_ (\chan -> control chan (Die msg)) chans
    mapM_ wait                              chans
