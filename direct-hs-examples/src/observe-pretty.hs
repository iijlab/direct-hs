import           Control.Applicative  ((<|>))
import qualified Data.ByteString.Lazy as B
import           System.Envy          (FromEnv, decodeEnv, env, fromEnv)
import           System.Exit          (die)
import           Text.Pretty.Simple   (pPrint)

import qualified Web.Direct           as D


newtype EndpointUrl = EndpointUrl String deriving Show

instance FromEnv EndpointUrl where
  fromEnv =
    EndpointUrl <$> (env "DIRECT_ENDPOINT_URL" <|> pure "wss://api.direct4b.com/albero-app-server/api")


main :: IO ()
main = do
    pInfo <- dieWhenLeft . D.deserializeLoginInfo =<< B.readFile jsonFileName
    (EndpointUrl url) <- dieWhenLeft =<< decodeEnv
    D.withClient
        D.defaultConfig
            { D.directEndpointUrl = url
            , D.directCreateMessageHandler = \_client msg _aux -> pPrint msg
            }
        pInfo
        (\_ -> return ())


dieWhenLeft :: Either String a -> IO a
dieWhenLeft = either exitError return


exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"


jsonFileName :: FilePath
jsonFileName = ".direct4b.json"