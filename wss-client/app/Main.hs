module Main where


import           Control.Monad (forever, unless)
import           Control.Concurrent  (forkIO, killThread)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.Environment (getArgs)
import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets as WS


main :: IO ()
main = do
  url <- head <$> getArgs
  WS.withWsClient url $ \conn -> do
    tid <- forkIO $ forever $ do
      msg <- WS.receiveData conn
      T.putStrLn msg

    let loop = do
          line <- T.getLine
          unless (T.null line) $
            WS.sendTextData conn line >> loop

    loop
    killThread tid
