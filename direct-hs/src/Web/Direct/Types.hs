module Web.Direct.Types where

import qualified Data.Text                     as T
import           Data.Word                                ( Word64 )

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
    } deriving (Eq, Show, Read)

-- | Type for domains.
data Domain = Domain {
      domainId   :: !DomainId
    , domainName :: !T.Text
    } deriving (Eq, Show, Read)

-- | Talk room types.
data TalkType = UnknownTalk | PairTalk | GroupTalk !T.Text deriving (Eq, Show, Read)

-- | Type for talk rooms.
data TalkRoom = TalkRoom {
      talkId      :: !TalkId
    , talkType    :: !TalkType
    , talkUserIds :: [UserId]
    } deriving (Eq, Show, Read)

-- | Type for Direct messages.
data Message =
      Txt       !T.Text
    | Location  !T.Text !T.Text -- Address, GoogleMap URL
    | Stamp     !Word64 !Word64 !(Maybe T.Text)
    | YesNoQ    !T.Text
    | YesNoA    !T.Text Bool
    | SelectQ   !T.Text ![T.Text]
    | SelectA   !T.Text ![T.Text] T.Text
    | TaskQ     !T.Text Bool -- False: anyone, True: everyone
    | TaskA     !T.Text Bool Bool -- done
    | Files     ![File] !(Maybe T.Text)
    | Other     !T.Text
    deriving (Eq, Show)

data File = File
    { fileUrl         :: !T.Text
    , fileContentType :: !T.Text
    , fileContentSize :: !Word64
    , fileName        :: !T.Text
    , fileId          :: !FileId
    } deriving (Eq, Show)

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
    }
