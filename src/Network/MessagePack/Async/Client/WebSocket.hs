{-# LANGUAGE OverloadedStrings #-}

module Network.MessagePack.Async.Client.WebSocket (
    withClient
  , parseWsUrl
  , EndpointUrl
  , module Network.MessagePack.Async.Client
  ) where

import           Control.Concurrent (killThread)
import qualified Control.Error as Err
import qualified Control.Exception as E
import           Data.Maybe (fromMaybe)
import qualified Data.Text as T
import           Network.Socket (withSocketsDo, PortNumber)
import           Network.URI (parseURI, URI(..), URIAuth(..))
import qualified Network.WebSockets as Ws
import           Text.Read (readMaybe)
import qualified Wuss as Wss

import           Network.MessagePack.Async.Client

withClient :: EndpointUrl -> Config -> (Client -> IO a) -> IO a
withClient (EndpointUrl sec host path port) config action =
  withSocketsDo $ runWs $ \conn -> do
    Ws.forkPingThread conn 30
    c <- newClient config (Ws.sendBinaryData conn) (Ws.receiveData conn)
    tid <- forkReceiverThread c config
    ( do
      returned <- action c
      Ws.sendClose conn ("Bye!" :: T.Text)
      return returned
      ) `E.finally` killThread tid
  where
    runWs =
      if sec
        then Wss.runSecureClient host port path
        else Ws.runClient host (fromIntegral port) path

data EndpointUrl =
  EndpointUrl
    { _endpointUrlIsSecure :: !Bool
    , _endpointUrlHost :: !String
    , _endpointUrlPath :: !String
    , _endpointUrlPort :: !PortNumber
    } deriving (Eq, Show)

parseWsUrl :: String -> Either String EndpointUrl
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
    noteInvalidUrl :: String -> Maybe a -> Either String a
    noteInvalidUrl msg = Err.note (msg ++ ": " ++ show raw)

    dieWhenEmpty :: String -> String -> Either String String
    dieWhenEmpty msg "" = Left (msg ++ ": " ++ show raw)
    dieWhenEmpty _ s = return s
