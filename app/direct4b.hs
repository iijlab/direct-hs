{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative    ((<**>), (<|>))
import           Control.Concurrent     (threadDelay)
import qualified Control.Exception      as E
import           Control.Monad          (forever, join, forM_)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy   as B
import           Data.List              (intercalate)
import qualified Data.MessagePack       as M
import qualified Data.MessagePack.RPC   as Msg
import           Data.Monoid            ((<>))
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.IO      as TL
import qualified Options.Applicative    as Opt
import qualified System.Directory       as Dir
import           System.Envy            (FromEnv, decodeEnv, env, fromEnv)
import           System.Exit            (die)
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (NoBuffering), hGetEcho,
                                         hSetBuffering, hSetEcho, stderr, stdin,
                                         stdout)

import qualified Network.MessagePack.Async.Client.WebSocket as Rpc
import qualified Web.Direct             as D

main :: IO ()
main = join $ Opt.execParser optionsInfo
  where
    optionsInfo :: Opt.ParserInfo (IO ())
    optionsInfo = Opt.info
        (options <**> Opt.helper)
        (  Opt.fullDesc
        <> Opt.progDesc "Command line client for direct4b.com"
        <> Opt.header ""
        )

    options :: Opt.Parser (IO ())
    options =
        Opt.subparser
            $  Opt.command "login" (Opt.info (pure login) Opt.briefDesc)
            <> Opt.command
                   "send"
                   (Opt.info
                       (   sendText
                       <$> Opt.argument Opt.auto (Opt.metavar "TALK_ID")
                       )
                       (Opt.fullDesc <> Opt.progDesc
                           "Send a message from stdin as the logged-in user."
                       )
                   )
            <> Opt.command
                   "observe"
                   (Opt.info
                       (pure observe)
                       (Opt.fullDesc <> Opt.progDesc
                           "Observe all messages for the logged-in user."
                       )
                   )


newtype EndpointUrl = EndpointUrl { getEndpointUrl :: String } deriving Show

instance FromEnv EndpointUrl where
  fromEnv =
    EndpointUrl <$> (env "DIRECT_ENDPOINT_URL" <|> pure "wss://api.direct4b.com/albero-app-server/api")


data Env =
  Env
    { directEmailAddress :: T.Text
    , directPassword     :: T.Text
    , directEndpointUrl  :: String
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
    let url = directEndpointUrl e
    putStrLn $ "Parsed URL:" ++ show url

    D.withAnonymousClient url Rpc.defaultConfig $ \ac -> do
        c <- throwWhenLeft
            =<< D.login ac (directEmailAddress e) (directPassword e)
        putStrLn "Successfully logged in."

        B.writeFile jsonFileName
            $ D.serializePersistedInfo
            $ D.clientPersistedInfo c
        cd <- Dir.getCurrentDirectory
        putStrLn $ "Saved access token at '" ++ (cd </> jsonFileName) ++ "'."


sendText :: D.TalkId -> IO ()
sendText tid = do
    txt   <- TL.stripEnd <$> TL.getContents
    pInfo <-
        dieWhenLeft . D.deserializePersistedInfo =<< B.readFile jsonFileName
    (EndpointUrl url) <- dieWhenLeft =<< decodeEnv
    D.withClient url pInfo D.defaultConfig $ \client -> do
        forM_ (TL.chunksOf 1024 txt) $ \chunk ->
            D.sendMessage client $ D.Txt tid $ TL.toStrict chunk

observe :: IO ()
observe = do
    pInfo <-
        dieWhenLeft . D.deserializePersistedInfo =<< B.readFile jsonFileName
    (EndpointUrl url) <- dieWhenLeft =<< decodeEnv
    D.withClient
        url
        pInfo
        D.defaultConfig { D.directLogger = putStrLn, D.directFormatter = showMsg }
      -- `forever $ return ()` doesn't give up control flow to the receiver thread.
        (\_ -> forever $ threadDelay $ 10 * 1000)

throwWhenLeft :: E.Exception e => Either e a -> IO a
throwWhenLeft = either E.throwIO return

dieWhenLeft :: Either String a -> IO a
dieWhenLeft = either exitError return

exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"

jsonFileName :: FilePath
jsonFileName = ".direct4b.json"

showObjs :: [M.Object] -> String
showObjs objs = "[" ++ intercalate "," (map showObj objs) ++ "]"

showObj :: M.Object -> String
showObj (M.ObjectWord w)  = "+" ++ show w
showObj (M.ObjectInt  n)  = show n
showObj M.ObjectNil       = "nil"
showObj (M.ObjectBool  b) = show b
showObj (M.ObjectStr   s) = "\"" ++ T.unpack s ++ "\""
showObj (M.ObjectArray v) = "[" ++ intercalate "," (map showObj v) ++ "]"
showObj (M.ObjectMap   m) = "{" ++ intercalate "," (map showPair m) ++ "}"
    where showPair (x, y) = "(" ++ showObj x ++ "," ++ showObj y ++ ")"
showObj (M.ObjectBin _   ) = error "ObjectBin"
showObj (M.ObjectExt _ _ ) = error "ObjectExt"
showObj (M.ObjectFloat  _) = error "ObjectFloat"
showObj (M.ObjectDouble _) = error "ObjectDouble"

showMsg :: Msg.Message -> String
showMsg (Msg.RequestMessage _ method objs) =
    "request " ++ T.unpack method ++ " " ++ showObjs objs
showMsg (Msg.ResponseMessage _ (Left  obj)) = "response error " ++ showObj obj
showMsg (Msg.ResponseMessage _ (Right obj)) = "response " ++ showObj obj
showMsg (Msg.NotificationMessage method objs) =
    "notification " ++ T.unpack method ++ " " ++ showObjs objs
