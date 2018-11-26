module Web.Direct.Client.Channel
    ( withChannel'
    , ChannelDB
    , newChannelDB
    , shutdown'
    , findChannel'
    -- re-exporting
    , Channel
    , send
    , recv
    , ChannelType
    , pairChannel
    , pinPointChannel
    , groupChannel
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
import           Web.Direct.DirectRPC                     hiding (getDomains)
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
    -> Domain
    -> ChannelDB
    -> StatusVar
    -> ChannelType
    -> IO (Maybe Channel)
allocateChannel rpcclient dom chanDB tvar ctyp = do
    (room, muser) <- case ctyp of
        Pair user -> do
            room <- createPairTalk rpcclient dom user
            return (room, Just user)
        PinPoint room user -> return (room, Just user)
        Group room         -> return (room, Nothing)
    let ckey = (talkId room, userId <$> muser)
    chan <- newChannel rpcclient ctyp ckey
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

allChannels :: ChannelDB -> IO [Channel]
allChannels chanDB = HM.elems <$> S.atomically (S.readTVar chanDB)

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
    -> Domain
    -> ChannelDB
    -> StatusVar
    -> ChannelType
    -> (Channel -> IO ())
    -> IO Bool
withChannel' rpcclient dom chanDB tvar ctyp body = do
    mchan <- allocateChannel rpcclient dom chanDB tvar ctyp
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
