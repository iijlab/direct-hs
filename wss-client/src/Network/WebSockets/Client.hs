-- | A-little-bit-higher-level WebSocket client library.
--
--   Thanks to [http-client](https://hackage.haskell.org/package/http-client) and [http-client-tls](https://hackage.haskell.org/package/http-client-tls), functions in this module support @HTTP_PROXY@ environment variable and TLS.
--
--   __NOTE__: Currently, non-TLS connection via an HTTP proxy server
--             is NOT supported.

module Network.WebSockets.Client
  ( withConnection

    -- * Re-export from Network.WebSockets
  , WS.Connection

    -- ** Sending and receiving messages
  , WS.receive
  , WS.receiveDataMessage
  , WS.receiveData
  , WS.send
  , WS.sendDataMessage
  , WS.sendDataMessages
  , WS.sendTextData
  , WS.sendTextDatas
  , WS.sendBinaryData
  , WS.sendBinaryDatas
  , WS.sendClose
  , WS.sendCloseCode
  , WS.sendPing

    -- ** WebSocket message types
  , WS.Message (..)
  , WS.ControlMessage (..)
  , WS.DataMessage (..)
  , WS.WebSocketsData (..)

    -- ** Exceptions
  , WS.HandshakeException (..)
  , WS.ConnectionException (..)

    -- ** Utilities
  , WS.withPingThread
  ) where


import qualified Control.Exception            as E
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as BSL
import qualified Network.HTTP.Client          as Http
import qualified Network.HTTP.Client.Internal as Http
import           Network.HTTP.Client.TLS      (tlsManagerSettings)
import           Network.URI                  (URI (..), URIAuth (..), parseURI)
import qualified Network.WebSockets           as WS
import qualified Network.WebSockets.Stream    as WS

-- | The main entrypoint to connect by the WebSocket protocol.
--   This function automatically closes the created connection
--   after exiting the action.
--
--   If @HTTP_PROXY@ environment variable is set,
--   The connection is automatically made via the HTTP proxy server
--   specified by the variable.
--
--   __NOTE__: Currently, non-TLS connection via an HTTP proxy server
--             is NOT supported.
withConnection
  :: String -- ^ Endpoint URL (e.g. wss:\/\/example.com\/path).
  -> (WS.Connection -> IO a) -- ^ Action using the 'WS.Connection'
  -> IO a
withConnection url action = do
  man <- Http.newManager tlsManagerSettings
  withConnectionFromManager man url action


withConnectionFromManager
  :: Http.Manager -> String -> (WS.Connection -> IO a) -> IO a
withConnectionFromManager man rawUrl action = do
  (isSecure, host, port, path) <- parseWsUrl rawUrl

  let httpUrl = (if isSecure then "https://" else "http://") ++ host ++ port ++ path
  req <- Http.parseRequest $ "GET " ++ httpUrl

  Http.withConnection req man $ \httpConn -> do
    E.bracket
      ( do
        let r = do
              bs <- Http.connectionRead httpConn
              return $ if BS.null bs then Nothing else Just bs

            w = maybe (Http.connectionClose httpConn)
                      (Http.connectionWrite httpConn . BSL.toStrict)
        WS.makeStream r w
      )
      WS.close
      ( \stream -> do
        -- TODO: configure WS.ConnectionOptions
        WS.runClientWithStream stream
                               host
                               path
                               WS.defaultConnectionOptions
                               []
                               action
      )


parseWsUrl :: String -> IO (Bool, String, String, String)
parseWsUrl raw = do
  uri  <- noteInvalidUrl "Invalid URL given" $ parseURI raw
  auth <- noteInvalidUrl "No authroity specified" $ uriAuthority uri
  host <- dieWhenEmpty "No host specified" $ uriRegName auth
  let wss      = "wss:"
      scheme'  = uriScheme uri
      scheme   = if null scheme' then wss else scheme'
      isSecure = scheme == wss
      path     = uriPath uri ++ uriQuery uri ++ uriFragment uri
  return (isSecure, host, uriPort auth, if null path then "/" else path)
 where
  noteInvalidUrl :: String -> Maybe a -> IO a
  noteInvalidUrl msg =
    maybe (E.throwIO $ Http.InvalidUrlException raw msg) return

  dieWhenEmpty :: String -> String -> IO String
  dieWhenEmpty msg "" = E.throwIO $ Http.InvalidUrlException raw msg
  dieWhenEmpty _   s  = return s
