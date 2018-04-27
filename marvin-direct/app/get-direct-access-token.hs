{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative ((<|>))
import           Control.Exception (finally, bracket)
import           Control.Monad.IO.Class (liftIO)
import           Data.Maybe (fromMaybe)
import qualified Data.MessagePack as MsgPack
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.UUID as Uuid
import           Network (withSocketsDo, PortNumber)
import qualified Network.WebSockets as Ws
import           System.Envy (FromEnv, fromEnv, env, decodeEnv)
import           System.Exit (die)
import           System.IO
                   ( stderr
                   , stdout
                   , stdin
                   , hSetBuffering
                   , hGetEcho
                   , hSetEcho
                   , BufferMode(NoBuffering)
                   )
import qualified System.Random.MWC as Random
import           Text.Read (readMaybe)
import           Network.URI (parseURI, URI(..), URIAuth(..))
import qualified Wuss as Wss


data Env =
  Env
    { directEmailAddress :: T.Text
    , directPassword :: T.Text
    , directEndpointUrl :: String
    } deriving Show

instance FromEnv Env where
  fromEnv =
    Env
      <$> (env "DIRECT_EMAIL_ADDRESS" <|> getEmailAddress)
      <*> (env "DIRECT_PASSWORD" <|> getPassword)
      <*> (env "DIRECT_ENDPOINT_URL" <|> pure "wss://api.direct4b.com/albero-app-server/api")
    where
      getEmailAddress = liftIO $ do
        putStr "Enter direct email: "
        T.getLine
      getPassword = liftIO $ do
        putStr "Enter direct password: "
        t <- bracket
          (hGetEcho stdin)
          (hSetEcho stdin)
          (const (hSetEcho stdin False >> T.getLine))
        putStrLn ""
        return t


main :: IO ()
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  e <- dieWhenLeft decodeEnv
  uri@(_scheme, host, path, port) <- parseWsUrl $ directEndpointUrl e
  putStrLn $ "Parsed URL:" ++ show uri

  withSocketsDo
    $ Wss.runSecureClient host port path
    $ \conn -> action conn e `finally` Ws.sendClose conn ("Bye!" :: T.Text)
  where
    action conn e = do
      idfv <- genIdfv
      -- TODO: Resize by the size of Integer
      let magicNumber = MsgPack.ObjectWord 0
          -- TODO: Increment number to handle response
          requestId = MsgPack.ObjectWord 0
          agentName = MsgPack.ObjectStr "bot"
          magicConstant = MsgPack.ObjectStr ""
          functionName = MsgPack.ObjectStr "create_access_token"
          payload = MsgPack.pack
            [ magicNumber
            , requestId
            , functionName
            , MsgPack.ObjectArray
                [ MsgPack.ObjectStr $ directEmailAddress e
                , MsgPack.ObjectStr $ directPassword e
                , MsgPack.ObjectStr idfv
                , agentName
                , magicConstant
                ]
            ]
      Ws.sendBinaryData conn payload
      res <- MsgPack.unpack @IO @MsgPack.Object =<< Ws.receiveData conn
      putStrLn $ "Server responded with: " ++ show res


parseWsUrl :: String -> IO (String, String, String, PortNumber)
parseWsUrl raw = do
  uri <- dieWhenNothing ("Invalid URL given: " ++ show raw) $ parseURI raw
  auth <- dieWhenNothing ("No authroity specified: " ++ show raw) $ uriAuthority uri
  host <- dieWhenEmpty ("No host specified: " ++ show raw) $ uriRegName auth
  let path = uriPath uri
      defaultScheme = "wss:"
      scheme' = uriScheme uri
      scheme = if null scheme' then defaultScheme else scheme'
      defaultPort = if scheme == defaultScheme then 443 else 80
  return
    ( scheme
    , host
    , path ++ uriQuery uri
    , fromMaybe defaultPort
        $ readMaybe
        $ drop 1 {- drop the first colon -}
        $ uriPort auth
    )


dieWhenNothing :: String -> Maybe a -> IO a
dieWhenNothing _ (Just other) = return other
dieWhenNothing emsg Nothing = exitError emsg


dieWhenEmpty :: String -> String -> IO String
dieWhenEmpty emsg s =
  if null s
    then exitError emsg
    else return s


dieWhenLeft :: IO (Either String a) -> IO a
dieWhenLeft = (either exitError return =<<)


exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"


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
