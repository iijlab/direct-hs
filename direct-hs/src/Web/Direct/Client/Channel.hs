module Web.Direct.Client.Channel
    ( withChannel'
    , ChannelDB
    , newChannelDB
    , haltChannel
    , shutdown'
    , getChannels
    , findChannel'
    -- re-exporting
    , Channel(..)
    , send
    , recv
    , dispatch
    , ChannelKey
    )
where

import qualified Control.Concurrent                       as C
import qualified Control.Concurrent.STM                   as S
import           Control.Monad                            (void)
import qualified Data.Map.Strict                          as HM
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Client.Channel.Types
import           Web.Direct.Client.Status
import           Web.Direct.Message
import           Web.Direct.Types

----------------------------------------------------------------

type ChannelDB = S.TVar (HM.Map ChannelKey Channel)

newChannelDB :: IO ChannelDB
newChannelDB = S.newTVarIO HM.empty

----------------------------------------------------------------

-- | Creating a new channel.
--   This returns 'Nothing' after 'shutdown'.
allocateChannel
    :: RPC.Client
    -> ChannelDB
    -> StatusVar
    -> TalkRoom
    -> Maybe User
    -> IO (Maybe Channel)
allocateChannel rpcclient chanDB tvar room userLimit = do
    let mUserId = userId <$> userLimit
    let ckey = (talkId room, mUserId)
    chan <- newChannel rpcclient room userLimit ckey
    S.atomically $ do
        active <- isActiveSTM tvar
        if active
            then do
                S.modifyTVar' chanDB $ HM.insert ckey chan
                return $ Just chan
            else return Nothing

freeChannel :: ChannelDB -> Channel -> IO ()
freeChannel chanDB chan = S.atomically $ S.modifyTVar' chanDB $ HM.delete key
    where key = channelKey chan

haltChannel :: ChannelDB -> Channel -> IO ()
haltChannel chanDB chan = do
    die Nothing chan
    freeChannel chanDB chan

allChannels :: ChannelDB -> IO [Channel]
allChannels chanDB = HM.elems <$> S.atomically (S.readTVar chanDB)

getChannels :: ChannelDB -> TalkId -> IO [Channel]
getChannels chanDB tid =
    filter ((tid ==) . channelTalkId) <$> allChannels chanDB

findChannel' :: ChannelDB -> ChannelKey -> IO (Maybe Channel)
findChannel' chanDB key = HM.lookup key <$> S.atomically (S.readTVar chanDB)

----------------------------------------------------------------

wait :: ChannelDB -> IO ()
wait chanDB = S.atomically $ do
    db <- S.readTVar chanDB
    S.check $ HM.null db

----------------------------------------------------------------

withChannel'
    :: RPC.Client
    -> ChannelDB
    -> StatusVar
    -> TalkRoom
    -> Maybe User
    -> (Channel -> IO ())
    -> IO Bool
withChannel' rpcclient chanDB tvar room userLimit body = do
    mchan <- allocateChannel rpcclient chanDB tvar room userLimit
    case mchan of
        Nothing   -> return False
        Just chan -> do
            void $ C.forkFinally (body chan) $ \e -> do
                case e of
                    Left ex -> print ex
                    _       -> return ()
                freeChannel chanDB chan
            return True

shutdown' :: RPC.Client -> ChannelDB -> StatusVar -> Message -> IO ()
shutdown' rpcclient chanDB tvar msg = do
    inactivate tvar
    chans <- allChannels chanDB
    mapM_ (die $ Just msg) chans
    wait chanDB
    RPC.shutdown rpcclient
