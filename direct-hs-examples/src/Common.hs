module Common (
    jsonFileName
  , dieWhenLeft
  , exitError
  ) where

import           System.Exit            (die)

----------------------------------------------------------------

jsonFileName :: FilePath
jsonFileName = ".direct4b.json"

----------------------------------------------------------------

dieWhenLeft :: Either String a -> IO a
dieWhenLeft = either exitError return

exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"
