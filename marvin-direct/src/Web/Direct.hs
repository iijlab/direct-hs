{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , deserializePersistedInfo

  , EndpointUrl(..)
  , parseWsUrl

  , Exception(..)

  , DirectInt64
  , TalkId

  , login
  , createMessage
  , observeMessages

  ) where


import           Control.Concurrent.STM
                   ( TVar
                   , TMVar
                   , newTVarIO
                   , modifyTVar'
                   , readTVar
                   , atomically
                   )
import qualified Control.Error as Err
import qualified Control.Exception as E
import           Control.Monad (forM, forever)
import qualified Data.Aeson as Json
import           Data.Aeson
                  ( FromJSON
                  , ToJSON
                  , fieldLabelModifier
                  )
import qualified Data.ByteString.Lazy as B
import qualified Data.Char as Char
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Maybe (fromMaybe)
import qualified Data.MessagePack as MsgPack
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           Data.Typeable (Typeable)
import qualified Data.UUID as Uuid
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           Network (withSocketsDo, PortNumber)
import qualified Network.WebSockets as Ws
import           Network.URI (parseURI, URI(..), URIAuth(..))
import qualified Numeric
import           Safe (atMay)
import qualified System.Random.MWC as Random
import           Text.Read (readMaybe)
import qualified Wuss as Wss


data AnonymousClient =
  AnonymousClient
    { anonymousClientConnection :: !Ws.Connection
    , anonymousClientSessionState :: !SessionState
    }


data Client =
  Client
    { clientPersistedInfo :: !PersistedInfo
    , clientConnection :: !Ws.Connection
    , clientSessionState :: !SessionState
    }

data EndpointUrl =
  EndpointUrl
    { endpointUrlIsSecure :: !Bool
    , endpointUrlHost :: !String
    , endpointUrlPath :: !String
    , endpointUrlPort :: !PortNumber
    } deriving (Eq, Show)

data SessionState =
  SessionState
    { lastRequestId :: TVar RequestId
    , _responseBuffer :: TVar (IntMap (TMVar MsgPack.Object))
    -- ^ RequestId をキーとて、レスポンス(MsgPack.Object)を置くための箱を持つ
    }

type RequestId = Word64

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
    { fieldLabelModifier = firstLower . drop (length @[] "PersistedInfo")
    }

firstLower :: String -> String
firstLower (x : xs) = Char.toLower x : xs
firstLower _ = error "firstLower: Assertion failed: empty string"


withAnonymousClient :: EndpointUrl -> (AnonymousClient -> IO a) -> IO a
withAnonymousClient ep = withSomeClient ep AnonymousClient


withClient :: EndpointUrl -> PersistedInfo -> (Client -> IO a) -> IO a
withClient ep pInfo action = withSomeClient ep (Client pInfo) $ \c -> do
  createSession c
  action c


withSomeClient
  :: EndpointUrl
  -> (Ws.Connection -> SessionState -> client)
  -> (client -> IO a)
  -> IO a
withSomeClient (EndpointUrl sec host path port) c action =
  withSocketsDo $ runWs $ \conn -> do
    Ws.forkPingThread conn 30
    (action . c conn =<< initSessionState)
      `E.finally` Ws.sendClose conn ("Bye!" :: T.Text)
  where
    runWs =
      if sec
        then Wss.runSecureClient host port path
        else Ws.runClient host (fromIntegral port) path


initSessionState :: IO SessionState
initSessionState = SessionState <$> newTVarIO 0 <*> newTVarIO IM.empty


observeMessages :: Client -> IO ()
observeMessages c = do
  _ <- rethrowingException $ callRpc
    (clientConnection c)
    (clientSessionState c)
    "reset_notification"
    []

  _ <- rethrowingException $ callRpc
    (clientConnection c)
    (clientSessionState c)
    "start_notification"
    []

  forever $ do
     obj <- MsgPack.unpack =<< Ws.receiveData (clientConnection c)
     print (obj :: MsgPack.Object)


createMessage :: Client -> TalkId -> TL.Text -> IO ()
createMessage c tid content = do
  let messageType = MsgPack.ObjectWord 1
  -- NOTE:
  --  direct-js internally splits the message by 1024 characters.
  --  So this library follows the behavior.
  res <- forM (TL.chunksOf 1024 content) $ \chunk ->
    rethrowingException $ callRpc
      (clientConnection c)
      (clientSessionState c)
      "create_message"
      [ MsgPack.toObject tid
      , messageType
      , MsgPack.ObjectStr $ TL.toStrict chunk
      ]
  -- TODO: Define type for the response, then delete the debug message
  putStrLn $ "Successfully sent a message. ResponseRecieved: " ++ show res


createSession :: Client -> IO ()
createSession c = do
  res <-
    rethrowingException $ callRpc
      (clientConnection c)
      (clientSessionState c)
      "create_session"
      [ MsgPack.ObjectStr $ persistedInfoDirectAccessToken $ clientPersistedInfo c
      , MsgPack.ObjectStr apiVersion
      , MsgPack.ObjectStr agentName
      ]
  -- TODO: Where to save login user info
  putStrLn $ "Successfully created a session. ResponseRecieved: " ++ show res


login
  :: AnonymousClient
  -> T.Text -- ^ Login email address for direct.
  -> T.Text -- ^ Login password for direct.
  -> IO (Either Exception Client)
login c email pass = do
  idfv <- genIdfv

  let magicConstant = MsgPack.ObjectStr ""
  res <-
    callRpc
      (anonymousClientConnection c)
      (anonymousClientSessionState c)
      "create_access_token"
      [ MsgPack.ObjectStr email
      , MsgPack.ObjectStr pass
      , MsgPack.ObjectStr idfv
      , MsgPack.ObjectStr agentName
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
      Left e -> return $ Left e
  -- Example no error: ObjectArray [ObjectWord 1,ObjectWord 0,ObjectNil,ObjectStr "..."]
  -- Example error:    ObjectArray [ObjectWord 1,ObjectWord 0,ObjectMap [(ObjectStr "code",ObjectWord 401),(ObjectStr "message",ObjectStr "invalid email or password")],ObjectNil]


rethrowingException :: IO MsgPack.Object -> IO MsgPack.Object
rethrowingException action = do
  res <- action
  case extractResult res of
      Right obj -> return obj
      Left e -> E.throwIO e


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
callRpc :: Ws.Connection -> SessionState -> T.Text -> [MsgPack.Object] -> IO MsgPack.Object
callRpc conn st funName args = do
  let magicNumber = MsgPack.ObjectWord 0
  requestId <- MsgPack.ObjectWord <$> getNewRequestId st
  putStr "Function name: "
  print funName
  putStr "Arguments: "
  print args
  putStr "Payload: "
  let p = MsgPack.pack
        [ magicNumber
        , requestId
        , MsgPack.ObjectStr funName
        , MsgPack.ObjectArray args
        ]
  putStrLn $ unwords $ map (($ "") . Numeric.showHex) $ B.unpack p
  Ws.sendBinaryData conn p
  MsgPack.unpack =<< Ws.receiveData conn

getNewRequestId :: SessionState -> IO Word64
getNewRequestId ss = atomically $ do
  let lastRequestIdVar = lastRequestId ss
  current <- readTVar lastRequestIdVar
  modifyTVar' lastRequestIdVar (+ 1)
  return current


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


agentName :: T.Text
agentName = "bot"


apiVersion :: T.Text
apiVersion = "1.91"
