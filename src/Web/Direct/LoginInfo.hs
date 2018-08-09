{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.LoginInfo (
    LoginInfo(..)
  , serializeLoginInfo
  , deserializeLoginInfo
  ) where

import           Data.Aeson           (FromJSON, ToJSON, fieldLabelModifier)
import qualified Data.Aeson           as Json
import qualified Data.ByteString.Lazy as B
import qualified Data.Char            as Char
import qualified Data.Text            as T
import           GHC.Generics         (Generic)

----------------------------------------------------------------

data LoginInfo = LoginInfo {
    loginInfoDirectAccessToken :: !T.Text
  , loginInfoIdfv              :: !T.Text
  } deriving (Eq, Show, Generic)

instance FromJSON LoginInfo where
  parseJSON = Json.genericParseJSON deriveJsonOptions

instance ToJSON LoginInfo where
  toJSON = Json.genericToJSON deriveJsonOptions
  toEncoding = Json.genericToEncoding deriveJsonOptions

serializeLoginInfo :: LoginInfo -> B.ByteString
serializeLoginInfo = Json.encode

deserializeLoginInfo :: B.ByteString -> Either String LoginInfo
deserializeLoginInfo = Json.eitherDecode

deriveJsonOptions :: Json.Options
deriveJsonOptions = Json.defaultOptions
    { fieldLabelModifier = firstLower . drop (T.length "LoginInfo")
    }

firstLower :: String -> String
firstLower (x : xs) = Char.toLower x : xs
firstLower _        = error "firstLower: Assertion failed: empty string"
