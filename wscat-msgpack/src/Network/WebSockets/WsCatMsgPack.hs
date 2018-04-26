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

            io $ print $ BS.unpack payload
            io (print =<< MsgPack.unpack @IO @MsgPack.Object payload)
            io $ putStrLn ""

            io $ Ws.sendBinaryData conn payload
            io (MsgPack.unpack @IO @MsgPack.Object =<< Ws.receiveData conn)

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
