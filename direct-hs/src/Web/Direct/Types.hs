module Web.Direct.Types where

import qualified Data.Text as T
import           Data.Word (Word64)

----------------------------------------------------------------

-- | Domain ID.
type DomainId           = Word64
-- | Talk room ID.
type TalkId             = Word64
-- | User ID.
type UserId             = Word64
-- | Mesage ID.
type MessageId          = Word64
-- | Timestamp
type Timestamp          = Word64
-- | Answer number of a 'SelectA'
type SelectAnswerNumber = Word64
-- | (Uploaded) File ID.
type FileId             = Word64
-- | (Uploaded) File size in bytes.
type FileSize           = Word64

----------------------------------------------------------------

-- | Type for users.
data User = User {
      userId                       :: !UserId
    , displayName                  :: !T.Text
    , canonicalDisplayName         :: !T.Text
    , phoneticDisplayName          :: !T.Text
    , canonicalPhoneticDisplayName :: !T.Text
    } deriving (Eq, Show, Read)

-- | Type for domains.
data Domain = Domain {
      domainId   :: !DomainId
    , domainName :: !T.Text
    } deriving (Eq, Show, Read)

-- | Talk room types.
data TalkType = UnknownTalk | PairTalk | GroupTalk !T.Text !TalkSettings deriving (Eq, Show, Read)

-- | Type for talk rooms.
data TalkRoom = TalkRoom {
      talkId        :: !TalkId
    , talkType      :: !TalkType
    , talkUserIds   :: [UserId]
    , talkUpdatedAt :: !Timestamp
    } deriving (Eq, Show, Read)

newtype TalkSettings =
    TalkSettings { allowDisplayPastMessages :: Bool } deriving (Eq, Show, Read)

data AnswerFor r q =
    AnswerFor { response :: !r, question :: !q, inReplyTo :: !MessageId } deriving (Eq, Show, Read)

data ClosingType = OnlyOne | Anyone deriving (Eq, Show, Read)

data Question q = Question !ClosingType !q deriving (Eq, Show, Read)

type Answer r q = AnswerFor r (Question q)

data SelectQuestion =
    SelectQuestion { selectQuestionText :: !T.Text,  selectQuestionOptions :: ![T.Text] } deriving (Eq, Show, Read)

-- | Type for Direct messages.
data Message =
      Txt       !T.Text
    | Location  !T.Text !T.Text -- Address, GoogleMap URL
    | Stamp     !Word64 !Word64 !(Maybe T.Text)
    | YesNoQ    !(Question T.Text)
    | YesNoA    !(Answer Bool T.Text)
    | SelectQ   !(Question SelectQuestion)
    | SelectA   !(Answer SelectAnswerNumber SelectQuestion)
    | TaskQ     !(Question T.Text)
    | TaskA     !(Answer Bool T.Text)
    | Files     ![File] !(Maybe T.Text)
    | Other     !T.Text
    deriving (Eq, Show, Read)

data File = File
    { fileUrl         :: !T.Text
    , fileContentType :: !T.Text
    , fileContentSize :: !Word64
    , fileName        :: !T.Text
    , fileId          :: !FileId
    } deriving (Eq, Show, Read)

-- | Created from the response of direct's RPC function @create_upload_auth@.
--   Contains information to upload/download a file to/from direct.
data UploadAuth = UploadAuth
    { uploadAuthGetUrl             :: !T.Text
    , uploadAuthPutUrl             :: !T.Text
    , uploadAuthFileId             :: !FileId
    , uploadAuthContentDisposition :: !T.Text
    } deriving (Eq, Show, Read)

data NotificationHandlers = NotificationHandlers
    { onNotifyCreateMessage :: Message -> MessageId -> TalkId -> UserId -> IO ()
    , onNotifyAddTalkers :: DomainId -> TalkRoom -> IO ()
    , onNotifyAddAcquaintance :: DomainId -> User -> IO ()
    , onNotifyDeleteTalk :: TalkId -> IO ()
    , onNotifyDeleteTalker :: DomainId -> TalkId -> [UserId] -> [UserId] -> IO ()
    , onNotifyDeleteAcquaintance :: DomainId -> UserId -> IO ()
    , onNotifyCreatePairTalk :: DomainId -> TalkRoom -> IO ()
    , onNotifyCreateGroupTalk :: DomainId -> TalkRoom -> IO ()
    }
