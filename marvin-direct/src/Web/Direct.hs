{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Web.Direct
  ( AnonymousClient
  , Client
  , clientPersistedInfo

  , withAnonymousClient
  , withClient

  , PersistedInfo(..)
  , serializePersistedInfo

  , EndpointUrl(..)
  , parseWsUrl

  , Exception(..)

  , login
  ) where


import qualified Control.Error as Err
import qualified Control.Exception as E
import qualified Data.Aeson as Json
import           Data.Aeson
                  ( FromJSON
                  , ToJSON
                  , fieldLabelModifier
                  )
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as Char
import qualified Data.IORef as IOR
import           Data.Maybe (fromMaybe)
import qualified Data.MessagePack as MsgPack
import qualified Data.Text as T
import           Data.Typeable (Typeable)
import qualified Data.UUID as Uuid
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Network (withSocketsDo, PortNumber)
import qualified Network.WebSockets as Ws
import           Network.URI (parseURI, URI(..), URIAuth(..))
import           Safe (atMay)
import qualified System.Random.MWC as Random
import           Text.Read (readMaybe)
import qualified Wuss as Wss


data AnonymousClient =
  AnonymousClient
    { anonymousClientConnection :: Ws.Connection
    , anonymousClientSessionState :: SessionState
    }


data Client =
  Client
    { clientPersistedInfo :: PersistedInfo
    , _clientConnection :: Ws.Connection
    , _clientSessionState :: SessionState
    }

data EndpointUrl =
  EndpointUrl
    { endpointUrlIsSecure :: Bool
    , endpointUrlHost :: String
    , endpointUrlPath :: String
    , endpointUrlPort :: PortNumber
    } deriving (Eq, Show)

newtype SessionState = SessionState (IOR.IORef RequestId)

type RequestId = Word64

data PersistedInfo =
  PersistedInfo
    { persistedInfoDirectAccessToken :: T.Text
    , persistedInfoIdfv :: T.Text
    } deriving (Eq, Show, Generic)

deriveJsonOptions :: Json.Options
deriveJsonOptions =
  Json.defaultOptions
    { fieldLabelModifier = firstLower . drop (length @[] "PersistedInfo")
    }

firstLower :: String -> String
firstLower (x:xs) = Char.toLower x : xs
firstLower _ = error "firstLower: Assertion failed: empty string"

instance FromJSON PersistedInfo where
  parseJSON = Json.genericParseJSON deriveJsonOptions

instance ToJSON PersistedInfo where
  toJSON = Json.genericToJSON deriveJsonOptions
  toEncoding = Json.genericToEncoding deriveJsonOptions

serializePersistedInfo :: Client -> B.ByteString
serializePersistedInfo = Json.encode . clientPersistedInfo


data Exception =
  InvalidEmailOrPassword
    | InvalidWsUrl String
    | UnexpectedReponse MsgPack.Object
    deriving (Eq, Show, Typeable)

instance E.Exception Exception


withAnonymousClient :: EndpointUrl -> (AnonymousClient -> IO a) -> IO a
withAnonymousClient ep = withSomeClient ep AnonymousClient


withClient :: EndpointUrl -> PersistedInfo -> (Client -> IO a) -> IO a
withClient ep pInfo = withSomeClient ep (Client pInfo)


withSomeClient
  :: EndpointUrl
  -> (Ws.Connection -> SessionState -> client)
  -> (client -> IO a)
  -> IO a
withSomeClient (EndpointUrl sec host path port) c action =
  withSocketsDo $ runWs $ \conn ->
    (action . c conn =<< initSessionState)
      `E.finally` Ws.sendClose conn ("Bye!" :: T.Text)
  where
    runWs =
      if sec
        then Wss.runSecureClient host port path
        else Ws.runClient host (fromIntegral port) path


initSessionState :: IO SessionState
initSessionState = SessionState <$> IOR.newIORef 0


login
  :: AnonymousClient
  -> T.Text -- ^ Login email address for direct.
  -> T.Text -- ^ Login password for direct.
  -> IO (Either Exception Client)
login c email pass = do
  idfv <- genIdfv

  let agentName = MsgPack.ObjectStr "bot"
      magicConstant = MsgPack.ObjectStr ""
  res <-
    callRpc
      (anonymousClientConnection c)
      (anonymousClientSessionState c)
      "create_access_token"
      [ MsgPack.ObjectStr email
      , MsgPack.ObjectStr pass
      , MsgPack.ObjectStr idfv
      , agentName
      , magicConstant
      ]
  case extractResult res of
      Right (MsgPack.ObjectStr token) ->
        return $ Right $ Client
          (PersistedInfo token idfv)
          (anonymousClientConnection c)
          (anonymousClientSessionState c)
      Right other ->
        return $ Left $ UnexpectedReponse other
      (Left e) -> return $ Left e
  -- Example no error: ObjectArray [ObjectWord 1,ObjectWord 0,ObjectNil,ObjectStr "..."]
  -- Example error:    ObjectArray [ObjectWord 1,ObjectWord 0,ObjectMap [(ObjectStr "code",ObjectWord 401),(ObjectStr "message",ObjectStr "invalid email or password")],ObjectNil]


extractResult :: MsgPack.Object -> Either Exception MsgPack.Object
extractResult res@(MsgPack.ObjectArray os) =
  fromMaybe (Left $ UnexpectedReponse res) mbRes
  where
    mbRes :: Maybe (Either Exception MsgPack.Object)
    mbRes = do
      secondO <- os `atMay` 2
      case secondO of
          MsgPack.ObjectNil -> Right <$> os `atMay` 3
          MsgPack.ObjectMap errorMap ->
            let isInvalidEP =
                  lookup (MsgPack.ObjectStr "message") errorMap
                    == Just (MsgPack.ObjectStr "invalid email or password")
            in
              return $ Left $
                if isInvalidEP
                  then InvalidEmailOrPassword
                  else UnexpectedReponse res
          other -> return $ Left $ UnexpectedReponse other
extractResult other = Left $ UnexpectedReponse other


-- TODO: async request/response handling using Control.Concurrent.Thread
-- TODO: Increment request id after sending
callRpc :: Ws.Connection -> SessionState -> T.Text -> [MsgPack.Object] -> IO MsgPack.Object
callRpc conn st funName args = do
  let magicNumber = MsgPack.ObjectWord 0
  requestId <- MsgPack.ObjectWord <$> getNewRequestId st
  Ws.sendBinaryData conn $ MsgPack.pack
    [ magicNumber
    , requestId
    , MsgPack.ObjectStr funName
    , MsgPack.ObjectArray args
    ]
  MsgPack.unpack =<< Ws.receiveData conn


getNewRequestId :: SessionState -> IO Word64
getNewRequestId (SessionState ior) =
  IOR.atomicModifyIORef' ior (\i -> (i + 1, i))


genIdfv :: IO T.Text
genIdfv = do
  g <- Random.createSystemRandom
  Uuid.toText
    <$> (
      Uuid.fromWords
        <$> Random.uniform g
        <*> Random.uniform g
        <*> Random.uniform g
        <*> Random.uniform g
    )


parseWsUrl :: String -> Either Exception EndpointUrl
parseWsUrl raw = do
  uri <- noteInvalidUrl "Invalid URL given" $ parseURI raw
  auth <- noteInvalidUrl "No authroity specified" $ uriAuthority uri
  host <- dieWhenEmpty "No host specified" $ uriRegName auth
  let path = uriPath uri
      wss = "wss:"
      scheme' = uriScheme uri
      scheme = if null scheme' then wss else scheme'
      isSecure = scheme == wss
      defaultPort = if isSecure then 443 else 80
  return $
    EndpointUrl
      isSecure
      host
      (path ++ uriQuery uri)
      ( fromMaybe defaultPort
          $ readMaybe
          $ drop 1 {- drop the first colon -}
          $ uriPort auth
      )

  where
    noteInvalidUrl :: String -> Maybe a -> Either Exception a
    noteInvalidUrl msg = Err.note $ InvalidWsUrl (msg ++ ": " ++ show raw)

    dieWhenEmpty :: String -> String -> Either Exception String
    dieWhenEmpty msg "" = Left $ InvalidWsUrl (msg ++ ": " ++ show raw)
    dieWhenEmpty _ s = return s
