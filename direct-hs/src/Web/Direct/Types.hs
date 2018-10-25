module Web.Direct.Types where

import qualified Data.Text as T
import           Data.Word (Word64)

----------------------------------------------------------------

-- | Domain ID.
type DomainId  = Word64
-- | Talk room ID.
type TalkId    = Word64
-- | User ID.
type UserId    = Word64
-- | Mesage ID.
type MessageId = Word64
-- | (Uploaded) File ID.
type FileId    = Word64
-- | (Uploaded) File size in bytes.
type FileSize  = Word64

----------------------------------------------------------------

-- | Type for users.
data User = User {
    userId                       :: !UserId
  , displayName                  :: !T.Text
  , canonicalDisplayName         :: !T.Text
  , phoneticDisplayName          :: !T.Text
  , canonicalPhoneticDisplayName :: !T.Text
  } deriving (Eq, Show)

-- | Type for domains.
data Domain = Domain {
    domainId   :: !DomainId
  , domainName :: !T.Text
  } deriving (Eq, Show)

-- | Talk room types.
data TalkType = UnknownTalk | PairTalk | GroupTalk !T.Text deriving (Eq, Show)

-- | Type for talk rooms.
data TalkRoom = TalkRoom {
    talkId    :: !TalkId
  , talkType  :: !TalkType
  , talkUsers :: [User] -- ^ The head of this list is myself.
  } deriving (Eq, Show)

-- | Created from the response of direct's RPC function @create_upload_auth@.
--   Contains information to upload/download a file to/from direct.
data UploadAuth = UploadAuth
    { uploadAuthGetUrl             :: !T.Text
    , uploadAuthPutUrl             :: !T.Text
    , uploadAuthFileId             :: !FileId
    , uploadAuthContentDisposition :: !T.Text
    }
