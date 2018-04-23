{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Network.WebSockets.WsCatMsgPack
  ( main
  ) where


import           Control.Error
                   ( fmapLT
                   , runExceptT
                   , ExceptT(ExceptT)
                   )
import           Control.Exception (try, SomeException)
import qualified Data.ByteString.Lazy as BS
import qualified Data.MessagePack as MsgPack
import           Data.Text (Text)
import           Network (withSocketsDo)
import qualified Network.WebSockets as Ws
import           System.Environment (getArgs)
import           System.Exit (die)
import           System.IO
                   ( isEOF
                   , stderr
                   , stdout
                   , stdin
                   , hPutStrLn
                   , hSetBuffering
                   , BufferMode(NoBuffering)
                   )
import qualified Wuss as Wss


main :: IO ()
main = do
  hSetBuffering stdin  NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering
  (host, path) <- parseArgs =<< getArgs
  putStrLn "Enter values:"

  withSocketsDo $ Wss.runSecureClient host 443 path loop
  where
    loop conn = do
      eof <- isEOF
      if eof
        then do
          Ws.sendClose conn ("Bye!" :: Text)
          putStrLn "Bye."
        else do
          putStr "> "
          response <- runExceptT $ do
            payload <- MsgPack.pack <$> io (readLn :: IO MsgPack.Object)
            {-
            let payload = BS.pack
                  [148,0,0,179,99,114,101,97,116,101,95,97,99,99,101,115,115,95,116,111,107,101,110,149,181,116,101,115,116,98,111,116,48,64,105,105,106,45,105,105,46,99,111,46,106,112,180,85,84,115,51,83,112,54,90,82,50,85,50,117,76,81,65,75,89,97,120,218,0,36,102,49,52,55,57,51,51,54,45,51,51,54,57,45,52,101,51,48,45,56,50,97,57,45,54,50,98,50,97,55,100,100,56,101,102,54,163,98,111,116,160]
            -}

            io $ print $ BS.unpack payload
            io $ print (MsgPack.unpack payload :: Either String MsgPack.Object)
            io $ putStrLn ""

            io $ Ws.sendBinaryData conn payload
            io $ Ws.receive conn

          case response of
              Right dat ->
                putStrLn $ "Server responded with: " ++ show dat
              Left emsg ->
                warn emsg
          loop conn


warn :: String -> IO ()
warn = hPutStrLn stderr . ("[ERROR] " ++)


io :: IO a -> ExceptT String IO a
io act =
  fmapLT (show @SomeException) $ ExceptT $ try act


-- TODO: parse endpoint URL
parseArgs :: [String] -> IO (String, String)
parseArgs [host, path] = return (host, path)
parseArgs other =
  die
    $ "[ERROR] Specify a host name and a path as command line arguments!\n"
      ++ "You actually gave " ++ show other ++ "."
