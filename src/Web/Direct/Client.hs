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
  , Channel
  , withChannel
  , findChannel
  , dispatch
  , shutdown
  , sendMessage
  , send
  , recv
  ) where

import qualified Control.Concurrent                       as C
import qualified Control.Concurrent.STM                   as S
import qualified Control.Exception                        as E
import           Control.Monad                            (void)
import qualified Data.HashMap.Strict                      as HM
import qualified Data.IORef                               as I
import qualified Data.MessagePack                         as M
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Exception
import           Web.Direct.Message
import           Web.Direct.PersistedInfo
import           Web.Direct.Types

----------------------------------------------------------------

data Status = Active | Inactive deriving Eq

type ChannelKey = (TalkId, UserId)

data Channel = Channel {
      toWorker      :: C.MVar (Either Control (Message, Aux))
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
  , clientChannels      :: S.TVar (HM.HashMap ChannelKey Channel)
  , clientStatus        :: S.TVar Status
  }

newClient :: PersistedInfo -> RPC.Client -> IO Client
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

isActive :: Client -> IO Bool
isActive client = S.atomically $ isActiveSTM client

isActiveSTM :: Client -> S.STM Bool
isActiveSTM client = (== Active) <$> S.readTVar (clientStatus client)

inactivate :: Client -> IO ()
inactivate client = S.atomically $ S.writeTVar (clientStatus client) Inactive

----------------------------------------------------------------

fromAux :: Aux -> ChannelKey
fromAux (Aux tid _ uid) = (tid, uid)

-- | Creating a new channel.
--   This returns 'Nothing' after 'shutdown'.
newChannel :: Client -> Aux -> IO (Maybe Channel)
newChannel client aux = do
    mvar <- C.newEmptyMVar
    let chan = Channel
            { toWorker      = mvar
            , channelClient = client
            , channelAux    = aux
            }
    S.atomically $ do
        active <- isActiveSTM client
        if active then do
          S.modifyTVar' chanDB $ HM.insert key chan
          return $ Just chan
        else
          return Nothing
  where
    chanDB = clientChannels client
    key = fromAux aux

freeChannel :: Client -> Aux -> IO ()
freeChannel client aux = S.atomically $
    S.modifyTVar' chanDB $ HM.delete key
  where
    chanDB = clientChannels client
    key = fromAux aux

allChannels :: Client -> IO [Channel]
allChannels client = HM.elems <$> S.atomically (S.readTVar chanDB)
  where
    chanDB = clientChannels client

findChannel :: Client -> Aux -> IO (Maybe Channel)
findChannel client aux = HM.lookup key <$> S.atomically (S.readTVar chanDB)
  where
    chanDB = clientChannels client
    key = fromAux aux

----------------------------------------------------------------

dispatch :: Channel -> Message -> Aux -> IO ()
dispatch chan msg aux = C.putMVar (toWorker chan) $ Right (msg, aux)

control :: Channel -> Control -> IO ()
control chan ctl = C.putMVar (toWorker chan) $ Left ctl

wait :: Client -> IO ()
wait client = S.atomically $ do
    db <- S.readTVar chanDB
    S.check $ HM.null db
  where
    chanDB = clientChannels client

----------------------------------------------------------------

sendMessage :: Client -> Message -> Aux -> IO (Either Exception MessageId)
sendMessage client req aux = do
    let obj        = encodeMessage req aux
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
withChannel :: Client -> Aux -> (Channel -> IO ()) -> IO Bool
withChannel client aux body = do
    mchan <- newChannel client aux
    case mchan of
      Nothing   -> return False
      Just chan -> do
          void $ C.forkFinally (body chan) $ \_ ->
              freeChannel client aux
          return True

recv :: Channel -> IO (Message, Aux)
recv chan = do
    cm <- C.takeMVar $ toWorker chan
    case cm of
        Right msg            -> return msg
        Left  (Die announce) -> do
            void $ sendMessage (channelClient chan) announce (channelAux chan)
            E.throwIO E.ThreadKilled

send :: Channel -> Message -> IO (Either Exception MessageId)
send chan msg = sendMessage (channelClient chan) msg (channelAux chan)

-- | This function lets the handler to not accept any message,
--   then sends the maintenance message to all channels,
--   and finnaly waits that all channels are closed.
shutdown :: Client -> Message -> IO ()
shutdown client msg = do
    inactivate client
    chans <- allChannels client
    mapM_ (\chan -> control chan (Die msg)) chans
    wait client
