{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Types where

import qualified Control.Exception                as E
import           Data.Aeson                       (FromJSON, ToJSON,
                                                   fieldLabelModifier)
import qualified Data.Aeson                       as Json
import qualified Data.ByteString.Lazy             as B
import qualified Data.Char                        as Char
import qualified Data.MessagePack                 as M
import qualified Data.Text                        as T
import           Data.Typeable                    (Typeable)
import           Data.Word                        (Word64)
import           GHC.Generics                     (Generic)

import qualified Network.MessagePack.Async.Client as Rpc

-- | Direct client.
data Client = Client {
    clientPersistedInfo :: !PersistedInfo
  , clientRpcClient     :: !AnonymousClient
  }

-- | Direct client not logined yet.
type AnonymousClient = Rpc.Client

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

type DirectInt64 = Word64

type TalkId = DirectInt64


deriveJsonOptions :: Json.Options
deriveJsonOptions = Json.defaultOptions
    { fieldLabelModifier = firstLower . drop (T.length "PersistedInfo")
    }

firstLower :: String -> String
firstLower (x:xs) = Char.toLower x : xs
firstLower _      = error "firstLower: Assertion failed: empty string"

data Request =
    ReqText   !T.Text
  | ReqStamp  !Word64 !DirectInt64
  | ReqYesNo  !T.Text
  | ReqSelect !T.Text ![T.Text]
  | ReqTask   !T.Text Bool -- False: anyone, True: everyone

data Response =
    RspText   !TalkId !T.Text
  | RspStamp  !TalkId !Word64 !DirectInt64
  | RspYesNo  !TalkId !T.Text Bool
  | RspSelect !TalkId !T.Text ![T.Text] T.Text
  | RspTask   !TalkId !T.Text Bool Bool -- done

type RspInfo = [(M.Object, M.Object)]

decodeResponse :: RspInfo -> Maybe Response
decodeResponse rspinfo = do
    M.ObjectWord tid <- look "talk_id" rspinfo
    typ              <- look "type" rspinfo
    case typ of
        M.ObjectWord 1 -> do
            msg <- look "content" rspinfo >>= M.fromObject
            Just $ RspText tid msg
        M.ObjectWord 2 -> do
            set <- look "stamp_set" rspinfo >>= M.fromObject
            idx <- look "stamp_index" rspinfo >>= M.fromObject
            Just $ RspStamp tid set idx
        M.ObjectWord 501 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            yon           <- look "response" m >>= M.fromObject
            Just $ RspYesNo tid qst yon
        M.ObjectWord 503 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            opt           <- look "options" m >>= M.fromObject
            idx           <- look "response" m >>= M.fromObject
            let ans = opt !! fromIntegral (idx :: Word64)
            Just $ RspSelect tid qst opt ans
        M.ObjectWord 505 -> do
            M.ObjectMap m <- look "content" rspinfo
            ttl           <- look "title" m >>= M.fromObject
            cls'          <- look "closing_type" m >>= M.fromObject
            don           <- look "done" m >>= M.fromObject
            let cls = if cls' == (1 :: Word64) then True else False
            Just $ RspTask tid ttl cls don
        _ -> Nothing
    where look key = lookup (M.ObjectStr key)

encodeRequest :: Request -> [M.Object]
encodeRequest (ReqText text) = [M.ObjectWord 1, M.ObjectStr text]
encodeRequest (ReqStamp s n) =
    [ M.ObjectWord 2
    , M.ObjectMap
        [ (M.ObjectStr "stamp_set"  , M.ObjectWord s)
        , (M.ObjectStr "stamp_index", M.toObject n)
        ]
    ]
encodeRequest (ReqYesNo q) =
    [ M.ObjectWord 500
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr q)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeRequest (ReqSelect q as) =
    [ M.ObjectWord 502
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr q)
        , (M.ObjectStr "options" , M.toObject as)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeRequest (ReqTask ttl cls) =
    [ M.ObjectWord 504
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        ]
    ]
