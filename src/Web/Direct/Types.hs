module Web.Direct.Types where

import qualified Data.Text as T
import           Data.Word (Word64)

----------------------------------------------------------------

type DomainId  = Word64
type TalkId    = Word64
type UserId    = Word64
type MessageId = Word64

----------------------------------------------------------------

data User = User {
    userId                       :: !UserId
  , displayName                  :: !T.Text
  , canonicalDisplayName         :: !T.Text
  , phoneticDisplayName          :: !T.Text
  , canonicalPhoneticDisplayName :: !T.Text
  }

data Domain = Domain {
    domainId   :: !DomainId
  , domainName :: !T.Text
  }

data TalkType = UnknownTalk | PairTalk | GroupTalk !T.Text deriving (Eq, Show)

data TalkRoom = TalkRoom {
    talkId    :: !TalkId
  , talkType  :: !TalkType
  , talkUsers :: [UserId]
  }
