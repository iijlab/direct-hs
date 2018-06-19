{-# LANGUAGE DeriveGeneric #-}

module Web.Direct.Types where

import qualified Control.Exception as E
import qualified Data.Aeson as Json
import           Data.Aeson
                  ( FromJSON
                  , ToJSON
                  , fieldLabelModifier
                  )
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as Char
import qualified Data.MessagePack as MsgPack
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import           Data.Word (Word64)
import           GHC.Generics (Generic)

import qualified Network.MessagePack.Async.Client as Rpc

-- | Direct client.
data Client =
  Client
    { clientPersistedInfo :: !PersistedInfo
    , clientRpcClient :: !AnonymousClient
    }

-- | Direct client not logined yet.
type AnonymousClient = Rpc.Client

data PersistedInfo =
  PersistedInfo
    { persistedInfoDirectAccessToken :: !T.Text
    , persistedInfoIdfv :: !T.Text
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
    | UnexpectedReponse !MsgPack.Object
    deriving (Eq, Show, Typeable)

instance E.Exception Exception


type DirectInt64 = Word64

type TalkId = DirectInt64


deriveJsonOptions :: Json.Options
deriveJsonOptions =
  Json.defaultOptions
    { fieldLabelModifier = firstLower . drop (length "PersistedInfo")
    }

firstLower :: String -> String
firstLower (x : xs) = Char.toLower x : xs
firstLower _ = error "firstLower: Assertion failed: empty string"
