module Web.Direct.Types where

import qualified Control.Exception                as E
import qualified Data.MessagePack                 as M
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Data.Word                        (Word64)

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
  , canonicalPhoneticIisplayName :: !T.Text
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
----------------------------------------------------------------

data Exception =
      InvalidEmailOrPassword
    | InvalidWsUrl !String
    | UnexpectedReponse !M.Object
  deriving (Eq, Show, Typeable)

instance E.Exception Exception
