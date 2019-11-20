{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Web.Direct.Types where

import qualified Data.MessagePack.Types as M
import qualified Data.Text              as T
import           Data.Word              (Word64)
import           GHC.Generics           (Generic)

----------------------------------------------------------------

-- | Domain ID.
type DomainId           = Word64
-- | Talk room ID.
type TalkId             = Word64
-- | User ID.
type UserId             = Word64
-- | Message ID.
type MessageId          = Word64
-- | Timestamp
type Timestamp          = Word64
-- | Answer number of a 'SelectA'
--   Actually values of this type are exchanged as unsigned integers,
--   but I chose 'Int' for compatibility with '!!'.
type SelectAnswerNumber = Int
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
    } deriving (Eq, Show, Read, Generic)

-- | Type for domains.
data Domain = Domain {
      domainId   :: !DomainId
    , domainName :: !T.Text
    } deriving (Eq, Show, Read, Generic)

-- | Talk room types.
data TalkType = UnknownTalk | PairTalk | GroupTalk !T.Text !TalkSettings deriving (Eq, Show, Read, Generic)

-- | Type for talk rooms.
data TalkRoom = TalkRoom {
      talkId        :: !TalkId
    , talkType      :: !TalkType
    , talkUserIds   :: [UserId]
    , talkUpdatedAt :: !Timestamp
    } deriving (Eq, Show, Read, Generic)

newtype TalkSettings =
    TalkSettings { allowDisplayPastMessages :: Bool } deriving (Eq, Show, Read, Generic)

-- | Type for Direct messages.
data Message =
      Txt       !T.Text
    | Location  !T.Text !T.Text -- Address, GoogleMap URL
    | Stamp     !Word64 !Word64 !(Maybe T.Text)
    | YesNoQ    !YesNoQuestion
    | YesNoA    !YesNoAnswer
    | SelectQ   !SelectQuestion
    | SelectA   !SelectAnswer
    | TaskQ     !TaskQuestion
    | TaskA     !TaskAnswer
    | Files     ![File] !(Maybe T.Text)
    | Other     !T.Text
    deriving (Eq, Show, Read, Generic)

data YesNoQuestion = YesNoQuestion
    { question :: !T.Text, closingType :: !ClosingType }
    deriving (Eq, Show, Read, Generic)

data YesNoAnswer = YesNoAnswer
    { question :: !T.Text, closingType :: !ClosingType, response :: !Bool, inReplyTo :: !MessageId }
    deriving (Eq, Show, Read, Generic)

data SelectQuestion = SelectQuestion
    { question :: !T.Text, options :: ![T.Text], closingType :: !ClosingType }
    deriving (Eq, Show, Read, Generic)

data SelectAnswer = SelectAnswer
    { question :: !T.Text, options :: ![T.Text], closingType :: !ClosingType, response :: !SelectAnswerNumber, inReplyTo :: !MessageId }
    deriving (Eq, Show, Read, Generic)

data TaskQuestion = TaskQuestion
    { title :: !T.Text, closingType :: !ClosingType }
    deriving (Eq, Show, Read, Generic)

data TaskAnswer = TaskAnswer
    { title :: !T.Text, closingType :: !ClosingType, done :: !Bool, inReplyTo :: !MessageId }
    deriving (Eq, Show, Read, Generic)

data ClosingType = OnlyOne | Anyone deriving (Eq, Show, Read, Generic)

instance M.MessagePack ClosingType where
    toObject OnlyOne = M.ObjectWord 0
    toObject Anyone  = M.ObjectWord 1

    fromObject m = do
        i <- M.fromObject m
        case i :: Int of
            0 -> return OnlyOne
            1 -> return Anyone
            _ -> fail $ "Unknown ClosingType: " ++ show i

data File = File
    { fileUrl         :: !T.Text
    , fileContentType :: !T.Text
    , fileContentSize :: !Word64
    , fileName        :: !T.Text
    , fileId          :: !FileId
    } deriving (Eq, Show, Read, Generic)

-- | Created from the response of direct's RPC function @create_upload_auth@.
--   Contains information to upload/download a file to/from direct.
data UploadAuth = UploadAuth
    { uploadAuthGetUrl             :: !T.Text
    , uploadAuthPutUrl             :: !T.Text
    , uploadAuthFileId             :: !FileId
    , uploadAuthContentDisposition :: !T.Text
    } deriving (Eq, Show, Read, Generic)

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
