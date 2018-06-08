{-# LANGUAGE OverloadedStrings #-}

module Main where

-- | Sample application of ws-client.
--   A simple command like wscat.


import           Control.Monad (forever, unless)
import           Control.Concurrent  (forkIO, killThread)
import qualified Data.ByteString.Char8 as B
import           System.Environment (getArgs)
import qualified Network.WebSockets.Client as WS
import qualified Network.WebSockets as WS


main :: IO ()
main = do
  url <- head <$> getArgs
  WS.withClient url $ \conn -> do
    tid <- forkIO $ forever $ do
      msg <- WS.receiveData conn
      B.putStrLn msg

    let loop = do
          line <- B.getLine
          unless (B.null line || line == "\r") $
            WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn $ B.pack "Bye!"
    killThread tid
