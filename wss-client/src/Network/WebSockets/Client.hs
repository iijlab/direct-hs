module Network.WebSockets.Client
  ( withWsClient
  ) where


import qualified Control.Exception as E
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Network.HTTP.Client as Http
import qualified Network.HTTP.Client.Internal as Http
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
import           Network.URI (parseURI, URI(..), URIAuth(..))


withWsClient :: String -> (WS.Connection -> IO a) -> IO a
withWsClient url action = do
  man <- Http.newManager tlsManagerSettings
  withWsClientFromManager man url action


withWsClientFromManager :: Http.Manager -> String -> (WS.Connection -> IO a) -> IO a
withWsClientFromManager man rawUrl action = do
  (isSecure, host, path) <- parseWsUrl rawUrl

  let httpUrl =
        (if isSecure then "https://" else "http://") ++ host ++ path
  req <- Http.parseRequest $ "GET " ++ httpUrl

  Http.withConnection req man $ \httpConn -> do
    E.bracket
      ( do
        let r = do
              bs <- Http.connectionRead $ httpConn
              return $
                if BS.null bs
                  then Nothing
                  else Just bs

            w =
              maybe
                (Http.connectionClose httpConn)
                (Http.connectionWrite httpConn . BSL.toStrict)
        WS.makeStream r w
      )
      WS.close
      (\stream ->
        -- TODO: configure WS.ConnectionOptions
        WS.runClientWithStream stream host path WS.defaultConnectionOptions [] $ \conn -> do
          action conn <* WS.sendClose conn (T.pack "Bye")
      )


parseWsUrl :: String -> IO (Bool, String, String)
parseWsUrl raw = do
  uri <- noteInvalidUrl "Invalid URL given" $ parseURI raw
  auth <- noteInvalidUrl "No authroity specified" $ uriAuthority uri
  host <- dieWhenEmpty "No host specified" $ uriRegName auth
  let wss = "wss:"
      scheme' = uriScheme uri
      scheme = if null scheme' then wss else scheme'
      isSecure = scheme == wss
  return (isSecure, host, uriPath uri ++ uriQuery uri ++ uriFragment uri)

  where
    noteInvalidUrl :: String -> Maybe a -> IO a
    noteInvalidUrl msg = maybe (E.throwIO $ Http.InvalidUrlException raw msg) return

    dieWhenEmpty :: String -> String -> IO String
    dieWhenEmpty msg "" = E.throwIO $ Http.InvalidUrlException raw msg
    dieWhenEmpty _ s = return s
