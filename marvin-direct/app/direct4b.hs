{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

import           Control.Applicative ((<|>), (<**>))
import qualified Control.Exception as E
import           Control.Monad (join)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as B
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
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
      Opt.subparser $ Opt.command "login" $ Opt.info (pure login) Opt.briefDesc


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
  url <- throwWhenLeft $ Direct.parseWsUrl $ directEndpointUrl e
  putStrLn $ "Parsed URL:" ++ show url

  Direct.withAnonymousClient url $ \ac -> do
    c <- throwWhenLeft =<<
      Direct.login ac (directEmailAddress e) (directPassword e)
    putStrLn "Successfully logged in."

    let path = ".direct4b.json"
    B.writeFile path $ Direct.serializePersistedInfo c
    cd <- Dir.getCurrentDirectory
    putStrLn $ "Saved access token at '" ++ (cd </> path) ++ "'."


throwWhenLeft :: E.Exception e => Either e a -> IO a
throwWhenLeft = either E.throwIO return


dieWhenLeft :: Either String a -> IO a
dieWhenLeft = either exitError return


exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"
