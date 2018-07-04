{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Types
  (
    Client(..)
  , newClient
  , setDomain
  , getDomain
  , setTalkRooms
  , getTalkRooms
  , setMe
  , getMe
  , setUsers
  , getUsers
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
  , encodeMessage
  , decodeMessage
  , Aux(..)
  , decodeUser
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

setDomain :: Client -> Domain -> IO ()
setDomain client domain = I.writeIORef (clientDomain client) (Just domain)

getDomain :: Client -> IO (Maybe Domain)
getDomain client = I.readIORef (clientDomain client)

setTalkRooms :: Client -> [TalkRoom] -> IO ()
setTalkRooms client talks = I.writeIORef (clientTalkRooms client) talks

getTalkRooms :: Client -> IO [TalkRoom]
getTalkRooms client = I.readIORef (clientTalkRooms client)

setMe :: Client -> User -> IO ()
setMe client user = I.writeIORef (clientMe client) (Just user)

getMe :: Client -> IO (Maybe User)
getMe client = I.readIORef (clientMe client)

setUsers :: Client -> [User] -> IO ()
setUsers client users = I.writeIORef (clientUsers client) users

getUsers :: Client -> IO [User]
getUsers client = I.readIORef (clientUsers client)

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
  , phoneticDisplayName          :: !T.Text
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
    Txt       !T.Text
  | Location  !T.Text !T.Text -- Address, GoogleMap URL
  | Stamp     !Word64 !Word64
  | YesNoQ    !T.Text
  | YesNoA    !T.Text Bool
  | SelectQ   !T.Text ![T.Text]
  | SelectA   !T.Text ![T.Text] T.Text
  | TaskQ     !T.Text Bool -- False: anyone, True: everyone
  | TaskA     !T.Text Bool Bool -- done
  | Other     !T.Text
  deriving (Eq, Show)

data Aux = Aux !TalkId !MessageId !UserId

decodeMessage :: [(M.Object, M.Object)] -> Maybe (Message, Aux)
decodeMessage rspinfo = do
    M.ObjectWord tid <- look "talk_id" rspinfo
    M.ObjectWord mid <- look "message_id" rspinfo
    M.ObjectWord uid <- look "user_id" rspinfo
    let aux = Aux tid mid uid
    typ              <- look "type" rspinfo
    case typ of
        M.ObjectWord 1 -> do
            text <- look "content" rspinfo >>= M.fromObject
            if "今ココ：" `T.isPrefixOf` text then
                let ln = T.lines text
                    addr = ln !! 1
                    url  = ln !! 2
                in Just (Location addr url, aux)
              else
                Just (Txt text, aux)
        M.ObjectWord 2 -> do
            set <- look "stamp_set" rspinfo >>= M.fromObject
            idx <- look "stamp_index" rspinfo >>= M.fromObject
            Just (Stamp set idx, aux)
        M.ObjectWord 500 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            Just (YesNoQ qst, aux)
        M.ObjectWord 501 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            ans           <- look "response" m >>= M.fromObject
            Just (YesNoA qst ans, aux)
        M.ObjectWord 502 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            opt           <- look "options" m >>= M.fromObject
            Just (SelectQ qst opt, aux)
        M.ObjectWord 503 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            opt           <- look "options" m >>= M.fromObject
            idx           <- look "response" m >>= M.fromObject
            let ans = opt !! fromIntegral (idx :: Word64)
            Just (SelectA qst opt ans, aux)
        M.ObjectWord 504 -> do
            M.ObjectMap m <- look "content" rspinfo
            ttl           <- look "title" m >>= M.fromObject
            cls'          <- look "closing_type" m >>= M.fromObject
            let cls = if cls' == (1 :: Word64) then True else False
            Just (TaskQ ttl cls, aux)
        M.ObjectWord 505 -> do
            M.ObjectMap m <- look "content" rspinfo
            ttl           <- look "title" m >>= M.fromObject
            cls'          <- look "closing_type" m >>= M.fromObject
            don           <- look "done" m >>= M.fromObject
            let cls = if cls' == (1 :: Word64) then True else False
            Just (TaskA ttl cls don, aux)
        _ -> Just (Other $ T.pack $ show rspinfo, aux)

look :: T.Text -> [(M.Object, a)] -> Maybe a
look key = lookup (M.ObjectStr key)

encodeMessage :: Message -> Aux -> [M.Object]
encodeMessage (Txt text) (Aux tid _ _) = [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]
encodeMessage (Location addr url) (Aux tid _ _) =
    [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr (T.unlines ["今ココ：",addr,url])]
encodeMessage (Stamp set idx) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 2
    , M.ObjectMap
        [ (M.ObjectStr "stamp_set"  , M.ObjectWord set)
        , (M.ObjectStr "stamp_index", M.ObjectWord idx)
        ]
    ]
encodeMessage (YesNoQ qst) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 500
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (YesNoA qst ans) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 501
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "response", M.ObjectBool ans)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectQ qst opt) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 502
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "options" , M.toObject opt)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectA qst opt ans) (Aux tid _ _) =
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
encodeMessage (TaskQ ttl cls) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 504
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        ]
    ]
encodeMessage (TaskA ttl cls don) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 505
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        , (M.ObjectStr "done"        , M.ObjectBool don)
        ]
    ]

encodeMessage (Other text) (Aux tid _ _) = [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]


decodeUser :: M.Object -> Maybe User
decodeUser (M.ObjectMap map) = do
    M.ObjectMap user <- look "user" map
    M.ObjectWord uid <- look "user_id" user
    M.ObjectStr eml <- look "email" user
    M.ObjectStr dname <- look "display_name" user
    M.ObjectStr cdname <- look "canonical_display_name" user
    M.ObjectStr pdname <- look "phonetic_display_name" user
    M.ObjectStr cpdname <- look "canonical_phonetic_display_name" user
    Just $ User uid eml dname cdname pdname cpdname
decodeUser _ = Nothing
