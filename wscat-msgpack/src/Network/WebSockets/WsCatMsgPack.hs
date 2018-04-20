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
import qualified Data.MessagePack as MsgPack
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

  Wss.runSecureClient host 443 path loop
  where
    loop conn = do
      eof <- isEOF
      if eof
        then do
          putStr "> "
          response <- runExceptT $ do
            payload <- MsgPack.pack <$> io (readLn :: IO MsgPack.Object)

            io $ Ws.sendBinaryData conn payload
            io $ Ws.receive conn

          case response of
              Right dat ->
                putStrLn $ "Server responded with: " ++ show dat
              Left emsg ->
                warn emsg
          loop conn
        else
          putStrLn "Bye."


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
