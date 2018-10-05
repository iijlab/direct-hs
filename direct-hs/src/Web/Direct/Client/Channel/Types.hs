module Web.Direct.Client.Channel.Types (
      Channel
    , channelTalkId
    , channelTalkRoom
    , newChannel
    , dispatch
    , die
    , send
    , recv
    , ChannelType(..)
    , pairChannel
    , pinPointChannel
    , groupChannel
    , ChannelKey
    , channelKey
    )
where

import qualified Control.Concurrent                       as C
import qualified Control.Exception                        as E
import           Control.Monad                            (void)
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.DirectRPC
import           Web.Direct.Exception
import           Web.Direct.Message
import           Web.Direct.Types

----------------------------------------------------------------

-- | A virtual communication channel.
data Channel = Channel {
      toWorker         :: C.MVar (Either Control (Message, MessageId))
    , channelRPCClient :: RPC.Client
    , channelType      :: ChannelType
    , channelKey       :: ChannelKey
    , channelTalkRoom  :: TalkRoom -- ^ Getting the talk room associated with this channel.
    }

channelTalkId :: Channel -> TalkId
channelTalkId = fst . channelKey

----------------------------------------------------------------

-- | Creating a new channel.
newChannel :: RPC.Client -> ChannelType -> ChannelKey -> TalkRoom -> IO Channel
newChannel rpcclient ctyp ckey room = do
    mvar <- C.newEmptyMVar
    return Channel
        { toWorker         = mvar
        , channelRPCClient = rpcclient
        , channelType      = ctyp
        , channelKey       = ckey
        , channelTalkRoom  = room
        }

----------------------------------------------------------------

dispatch :: Channel -> Message -> MessageId -> IO ()
dispatch chan msg mid = C.putMVar (toWorker chan) $ Right (msg, mid)

newtype Control = Die Message

control :: Channel -> Control -> IO ()
control chan ctl = C.putMVar (toWorker chan) $ Left ctl

die :: Message -> Channel -> IO ()
die msg chan = control chan (Die msg)

-- | Receiving a message from the channel.
recv :: Channel -> IO (Message, MessageId)
recv chan = do
    cm <- C.takeMVar $ toWorker chan
    case cm of
        Right msg            -> return msg
        Left  (Die announce) -> do
            void $ createMessage (channelRPCClient chan) announce $ channelTalkId chan
            E.throwIO E.ThreadKilled

-- | Sending a message to the channel.
send :: Channel -> Message -> IO (Either Exception MessageId)
send chan msg = createMessage (channelRPCClient chan) msg $ channelTalkId chan

----------------------------------------------------------------

-- | Type of channel.
data ChannelType = Pair               !User
                 | PinPoint !TalkRoom !User
                 | Group    !TalkRoom
                 deriving (Eq, Show)

-- | Pair channel with the user.
--   A pair talk room is created if necessary.
--   The conversation is NOT seen by other users.
pairChannel :: User -> ChannelType
pairChannel user = Pair user

-- | One-to-one channel with the user in the talk room.
--   The conversation is seen by other users.
pinPointChannel :: TalkRoom -> User -> ChannelType
pinPointChannel room user = PinPoint room user

-- | Group channel in the talk room.
groupChannel :: TalkRoom -> ChannelType
groupChannel room = Group room

type ChannelKey = (TalkId, Maybe UserId)
