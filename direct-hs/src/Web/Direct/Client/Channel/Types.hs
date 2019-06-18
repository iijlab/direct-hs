module Web.Direct.Client.Channel.Types
    ( Channel(..)
    , channelTalkId
    , newChannel
    , dispatch
    , die
    , send
    , recv
    , ChannelKey
    )
where

import qualified Control.Concurrent                       as C
import qualified Control.Exception                        as E
import           Control.Monad                            (void)
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.DirectRPC
import           Web.Direct.Exception
import           Web.Direct.Types

----------------------------------------------------------------

-- | A virtual communication channel.
data Channel = Channel {
      toWorker         :: C.MVar (Either Control (Message, MessageId, TalkRoom, User))
    , channelRPCClient :: RPC.Client
    , channelTalkRoom  :: TalkRoom
    , channelUserLimit :: Maybe User
    , channelKey       :: ChannelKey
    }

channelTalkId :: Channel -> TalkId
channelTalkId = fst . channelKey

----------------------------------------------------------------

-- | Creating a new channel.
newChannel :: RPC.Client -> TalkRoom -> Maybe User -> ChannelKey -> IO Channel
newChannel rpcclient room userLimit ckey = do
    mvar <- C.newEmptyMVar
    return Channel
        { toWorker         = mvar
        , channelRPCClient = rpcclient
        , channelTalkRoom  = room
        , channelUserLimit = userLimit
        , channelKey       = ckey
        }

----------------------------------------------------------------

dispatch :: Channel -> Message -> MessageId -> TalkRoom -> User -> IO ()
dispatch chan msg mid room user =
    C.putMVar (toWorker chan) $ Right (msg, mid, room, user)

newtype Control = Die (Maybe Message)

control :: Channel -> Control -> IO ()
control chan ctl = C.putMVar (toWorker chan) $ Left ctl

die :: Maybe Message -> Channel -> IO ()
die msg chan = control chan (Die msg)

-- | Receiving a message from the channel.
recv :: Channel -> IO (Message, MessageId, TalkRoom, User)
recv chan = do
    cm <- C.takeMVar $ toWorker chan
    case cm of
        Right msg                   -> return msg
        Left  (Die Nothing        ) -> E.throwIO E.ThreadKilled
        Left  (Die (Just announce)) -> do
            void
                $ createMessage (channelRPCClient chan) announce
                $ channelTalkId chan
            E.throwIO E.ThreadKilled

-- | Sending a message to the channel.
send :: Channel -> Message -> IO (Either Exception MessageId)
send chan msg = createMessage (channelRPCClient chan) msg $ channelTalkId chan

----------------------------------------------------------------

type ChannelKey = (TalkId, Maybe UserId)
