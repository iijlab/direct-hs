module Web.Direct.Channel (
      Channel
    , newChannel
    , dispatch
    , die
    , send
    , recv
    , ChannelType(..)
    , pairChannel
    , groupChannel
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
    }

----------------------------------------------------------------

-- | Creating a new channel.
newChannel :: RPC.Client -> ChannelType -> IO Channel
newChannel rpcclient ctyp = do
    mvar <- C.newEmptyMVar
    return Channel
        { toWorker         = mvar
        , channelRPCClient = rpcclient
        , channelType      = ctyp
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
            let tid = channelTalkId $ channelType chan
            void $ createMessage (channelRPCClient chan) announce tid
            E.throwIO E.ThreadKilled

-- | Sending a message to the channel.
send :: Channel -> Message -> IO (Either Exception MessageId)
send chan msg = createMessage (channelRPCClient chan) msg tid
    where tid = channelTalkId $ channelType chan

----------------------------------------------------------------

-- | Type of channel.
data ChannelType = Pair !TalkId !UserId
                 | Group !TalkId
                 deriving (Eq, Ord, Show)

channelTalkId :: ChannelType -> TalkId
channelTalkId (Pair tid _) = tid
channelTalkId (Group tid ) = tid

-- | Group channel based on 'TalkRoom'.
groupChannel :: TalkRoom -> ChannelType
groupChannel room = Group (talkId room)

-- | Pair channel based on 'TalkRoom' and 'User'.
pairChannel :: TalkRoom -> User -> ChannelType
pairChannel room user = Pair (talkId room) (userId user)
