{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative ((<|>), (<**>))
import           Control.Concurrent (threadDelay)
import qualified Control.Exception as E
import           Control.Monad (join, forever)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Options.Applicative as Opt
import qualified System.Directory as Dir
import           System.Envy (FromEnv, fromEnv, env, decodeEnv)
import           System.Exit (die)
import           System.FilePath ((</>))
import           System.IO
                   ( stderr
                   , stdout
                   , stdin
                   , hSetBuffering
                   , hGetEcho
                   , hSetEcho
                   , BufferMode(NoBuffering)
                   )

import qualified Web.Direct as Direct


main :: IO ()
main = join $ Opt.execParser optionsInfo
  where
    optionsInfo :: Opt.ParserInfo (IO ())
    optionsInfo =
      Opt.info
        (options <**> Opt.helper)
        (Opt.fullDesc <> Opt.progDesc "Command line client for direct4b.com" <> Opt.header "")

    options :: Opt.Parser (IO ())
    options =
      Opt.subparser $
        Opt.command "login" (Opt.info (pure login) Opt.briefDesc)
          <> Opt.command "send"
              ( Opt.info
                  (sendMessage <$> Opt.argument Opt.auto (Opt.metavar "TALK_ID"))
                  (Opt.fullDesc <> Opt.progDesc "Send a message from stdin as the logged-in user.")
              )
          <> Opt.command "observe"
              ( Opt.info
                  (pure observe)
                  (Opt.fullDesc <> Opt.progDesc "Observe all messages for the logged-in user.")
              )


newtype EndpointUrl = EndpointUrl { getEndpointUrl :: String } deriving Show

instance FromEnv EndpointUrl where
  fromEnv =
    EndpointUrl <$> (env "DIRECT_ENDPOINT_URL" <|> pure "wss://api.direct4b.com/albero-app-server/api")


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
      <*> (getEndpointUrl <$> fromEnv)
    where
      getEmailAddress = liftIO $ do
        putStr "Enter direct email: "
        T.getLine
      getPassword = liftIO $ do
        putStr "Enter direct password: "
        t <- E.bracket
          (hGetEcho stdin)
          (hSetEcho stdin)
          (const (hSetEcho stdin False >> T.getLine))
        putStrLn ""
        return t


login :: IO ()
login = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  e <- dieWhenLeft =<< decodeEnv
  url <- dieWhenLeft $ Direct.parseWsUrl $ directEndpointUrl e
  putStrLn $ "Parsed URL:" ++ show url

  Direct.withAnonymousClient url Direct.defaultConfig $ \ac -> do
    c <- throwWhenLeft =<<
      Direct.login ac (directEmailAddress e) (directPassword e)
    putStrLn "Successfully logged in."

    B.writeFile jsonFileName $ Direct.serializePersistedInfo $ Direct.clientPersistedInfo c
    cd <- Dir.getCurrentDirectory
    putStrLn $ "Saved access token at '" ++ (cd </> jsonFileName) ++ "'."


sendMessage :: Direct.DirectInt64 -> IO ()
sendMessage i64 = do
  msg <- TL.stripEnd <$> TL.getContents
  pInfo <- dieWhenLeft . Direct.deserializePersistedInfo =<< B.readFile jsonFileName
  (EndpointUrl surl) <- dieWhenLeft =<< decodeEnv
  url <- dieWhenLeft $ Direct.parseWsUrl surl
  Direct.withClient url pInfo Direct.defaultConfig $ \c -> Direct.createMessage c i64 msg

observe :: IO ()
observe = do
  pInfo <- dieWhenLeft . Direct.deserializePersistedInfo =<< B.readFile jsonFileName
  (EndpointUrl surl) <- dieWhenLeft =<< decodeEnv
  url <- dieWhenLeft $ Direct.parseWsUrl surl
  -- TODO: Handle Ctrl + C
  Direct.withClient
    url
    pInfo
    (Direct.defaultConfig { Direct.notificationHandler = showNotification })
    (\_ ->
      -- `forever $ return ()` doesn't give up control flow to the receiver thread.
      forever $ threadDelay $ 10 * 1000
    )
    where
      showNotification method params =
        putStrLn $ "method: " ++ show method ++ ", params: " ++ show params


throwWhenLeft :: E.Exception e => Either e a -> IO a
throwWhenLeft = either E.throwIO return


dieWhenLeft :: Either String a -> IO a
dieWhenLeft = either exitError return


exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"

jsonFileName :: FilePath
jsonFileName = ".direct4b.json"
