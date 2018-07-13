module Common (
    url
  , jsonFileName
  , dieWhenLeft
  , exitError
  ) where

import           System.Exit            (die)
import qualified Web.Direct             as D

----------------------------------------------------------------

url :: D.URL
url = "wss://api.direct4b.com/albero-app-server/api"

----------------------------------------------------------------

jsonFileName :: FilePath
jsonFileName = ".direct4b.json"

----------------------------------------------------------------

dieWhenLeft :: Either String a -> IO a
dieWhenLeft = either exitError return

exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"
