{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Types
  (
    Client(..)
  , newClient
  , PersistedInfo(..)
  , serializePersistedInfo
  , deserializePersistedInfo
  , Exception(..)
  , DomainId
  , TalkId
  , UserId
  , MessageId
  , Domain(..)
  , TalkRoom(..)
  , User(..)
  , Message(..)
  , messageTalkId
  , encodeMessage
  , decodeMessage
  ) where
import qualified Control.Exception                as E
import           Data.Aeson                       (FromJSON, ToJSON,
                                                   fieldLabelModifier)
import qualified Data.Aeson                       as Json
import qualified Data.ByteString.Lazy             as B
import qualified Data.Char                        as Char
import qualified Data.IORef                       as I
import           Data.List                        (elemIndex)
import qualified Data.MessagePack                 as M
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Data.Word                        (Word64)
import           GHC.Generics                     (Generic)

import qualified Network.MessagePack.Async.Client as Rpc

-- | Direct client.
data Client = Client {
    clientPersistedInfo :: !PersistedInfo
  , clientRpcClient     :: !Rpc.Client
  , clientDomain        :: I.IORef (Maybe Domain)
  , clientTalkRooms     :: I.IORef [TalkRoom]
  , clientMe            :: I.IORef (Maybe User)
  , clientUsers         :: I.IORef [User]
  }

newClient :: PersistedInfo -> Rpc.Client -> IO Client
newClient pinfo rpcClient =
    Client pinfo rpcClient <$> I.newIORef Nothing
                           <*> I.newIORef []
                           <*> I.newIORef Nothing
                           <*> I.newIORef []

data PersistedInfo = PersistedInfo {
    persistedInfoDirectAccessToken :: !T.Text
  , persistedInfoIdfv              :: !T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON PersistedInfo where
  parseJSON = Json.genericParseJSON deriveJsonOptions

instance ToJSON PersistedInfo where
  toJSON = Json.genericToJSON deriveJsonOptions
  toEncoding = Json.genericToEncoding deriveJsonOptions

serializePersistedInfo :: PersistedInfo -> B.ByteString
serializePersistedInfo = Json.encode

deserializePersistedInfo :: B.ByteString -> Either String PersistedInfo
deserializePersistedInfo = Json.eitherDecode


data Exception =
      InvalidEmailOrPassword
    | InvalidWsUrl !String
    | UnexpectedReponse !M.Object
  deriving (Eq, Show, Typeable)

instance E.Exception Exception

type DomainId  = Word64
type TalkId    = Word64
type UserId    = Word64
type MessageId = Word64

deriveJsonOptions :: Json.Options
deriveJsonOptions = Json.defaultOptions
    { fieldLabelModifier = firstLower . drop (T.length "PersistedInfo")
    }

firstLower :: String -> String
firstLower (x : xs) = Char.toLower x : xs
firstLower _        = error "firstLower: Assertion failed: empty string"

data User = User {
    userId                       :: !UserId
  , userEmail                    :: !T.Text
  , displayName                  :: !T.Text
  , canonicalDisplayName         :: !T.Text
  , phonetic_display_name        :: !T.Text
  , canonicalPhoneticIisplayName :: !T.Text
  }

data Domain = Domain {
    domainId   :: !DomainId
  , domainName :: !T.Text
  }

data TalkRoom = TalkRoom {
    talkId    :: !TalkId
  , talkName  :: !T.Text
  , talkUsers :: [User]
  }

data Message =
    Txt       !TalkId !T.Text
  | Location  !TalkId !T.Text !T.Text -- Address, GoogleMap URL
  | Stamp     !TalkId !Word64 !Word64
  | YesNoQ    !TalkId !T.Text
  | YesNoA    !TalkId !T.Text Bool
  | SelectQ   !TalkId !T.Text ![T.Text]
  | SelectA   !TalkId !T.Text ![T.Text] T.Text
  | TaskQ     !TalkId !T.Text Bool -- False: anyone, True: everyone
  | TaskA     !TalkId !T.Text Bool Bool -- done
  | Other     !TalkId !T.Text
  deriving (Eq, Show)

messageTalkId :: Message -> TalkId
messageTalkId (Txt       tid _)     = tid
messageTalkId (Location  tid _ _)   = tid
messageTalkId (Stamp     tid _ _)   = tid
messageTalkId (YesNoQ    tid _)     = tid
messageTalkId (YesNoA    tid _ _)   = tid
messageTalkId (SelectQ   tid _ _)   = tid
messageTalkId (SelectA   tid _ _ _) = tid
messageTalkId (TaskQ     tid _ _)   = tid
messageTalkId (TaskA     tid _ _ _) = tid
messageTalkId (Other     tid _)     = tid

type RspInfo = [(M.Object, M.Object)]

decodeMessage :: RspInfo -> Maybe Message
decodeMessage rspinfo = do
    M.ObjectWord tid <- look "talk_id" rspinfo
    typ              <- look "type" rspinfo
    case typ of
        M.ObjectWord 1 -> do
            text <- look "content" rspinfo >>= M.fromObject
            if "今ココ：" `T.isPrefixOf` text then
                let ln = T.lines text
                    addr = ln !! 1
                    url  = ln !! 2
                in Just $ Location tid addr url
              else
                Just $ Txt tid text
        M.ObjectWord 2 -> do
            set <- look "stamp_set" rspinfo >>= M.fromObject
            idx <- look "stamp_index" rspinfo >>= M.fromObject
            Just $ Stamp tid set idx
        M.ObjectWord 500 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            Just $ YesNoQ tid qst
        M.ObjectWord 501 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            ans           <- look "response" m >>= M.fromObject
            Just $ YesNoA tid qst ans
        M.ObjectWord 502 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            opt           <- look "options" m >>= M.fromObject
            Just $ SelectQ tid qst opt
        M.ObjectWord 503 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            opt           <- look "options" m >>= M.fromObject
            idx           <- look "response" m >>= M.fromObject
            let ans = opt !! fromIntegral (idx :: Word64)
            Just $ SelectA tid qst opt ans
        M.ObjectWord 504 -> do
            M.ObjectMap m <- look "content" rspinfo
            ttl           <- look "title" m >>= M.fromObject
            cls'          <- look "closing_type" m >>= M.fromObject
            let cls = if cls' == (1 :: Word64) then True else False
            Just $ TaskQ tid ttl cls
        M.ObjectWord 505 -> do
            M.ObjectMap m <- look "content" rspinfo
            ttl           <- look "title" m >>= M.fromObject
            cls'          <- look "closing_type" m >>= M.fromObject
            don           <- look "done" m >>= M.fromObject
            let cls = if cls' == (1 :: Word64) then True else False
            Just $ TaskA tid ttl cls don
        _ -> Just $ Other tid $ T.pack $ show rspinfo
    where look key = lookup (M.ObjectStr key)

encodeMessage :: Message -> [M.Object]
encodeMessage (Txt tid text) = [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]
encodeMessage (Location tid addr url) =
    [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr (T.unlines ["今ココ：",addr,url])]
encodeMessage (Stamp tid set idx) =
    [ M.ObjectWord tid
    , M.ObjectWord 2
    , M.ObjectMap
        [ (M.ObjectStr "stamp_set"  , M.ObjectWord set)
        , (M.ObjectStr "stamp_index", M.ObjectWord idx)
        ]
    ]
encodeMessage (YesNoQ tid qst) =
    [ M.ObjectWord tid
    , M.ObjectWord 500
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (YesNoA tid qst ans) =
    [ M.ObjectWord tid
    , M.ObjectWord 501
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "response", M.ObjectBool ans)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectQ tid qst opt) =
    [ M.ObjectWord tid
    , M.ObjectWord 502
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "options" , M.toObject opt)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectA tid qst opt ans) =
    [ M.ObjectWord tid
    , M.ObjectWord 503
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "options" , M.toObject opt)
        , (M.ObjectStr "response", M.ObjectWord (fromIntegral idx))
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
  where
    Just idx = ans `elemIndex` opt -- fixme
encodeMessage (TaskQ tid ttl cls) =
    [ M.ObjectWord tid
    , M.ObjectWord 504
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        ]
    ]
encodeMessage (TaskA tid ttl cls don) =
    [ M.ObjectWord tid
    , M.ObjectWord 505
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        , (M.ObjectStr "done"        , M.ObjectBool don)
        ]
    ]

encodeMessage (Other tid text) = [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]
