{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE StandaloneDeriving #-}

import           Control.Applicative      ((<**>))
import           Control.Arrow            (second)
import           Control.Concurrent       (threadDelay)
import           Control.Error            (note)
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Lazy     as B
import           Data.Char                (isSpace)
import           Data.List                (break)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Options.Applicative      as Opt
import qualified System.Console.Haskeline as Hl
import qualified System.FilePath          as FP
import           System.IO                (hFlush, hPutStrLn, stderr, stdout)
import           Text.Pretty.Simple       (pPrint, pShow)
import           Text.Read                (readMaybe)

import qualified Web.Direct               as D

import           Common

deriving instance Read D.File

deriving instance Read D.Message


data Args = Args
    { loginInfoFile :: FilePath
    , logMessage    :: Bool
    , dumpMsgpack   :: Bool
    } deriving (Eq, Show)


main :: IO ()
main = do
    args <- Opt.execParser argsInfo

    let jsonBaseName = FP.takeBaseName $ loginInfoFile args
        histFile     = jsonBaseName ++ ".history.txt"
    pInfo <- dieWhenLeft . D.deserializeLoginInfo =<< B.readFile (loginInfoFile args)
    hasT  <- Hl.runInputT Hl.defaultSettings Hl.haveTerminalUI
    let prompt = if hasT then consolePrompt jsonBaseName else ""

    D.withClient
        D.defaultConfig
            { D.directCreateMessageHandler     =
                if logMessage args
                  then
                    \_client msg ->
                        printMessage msg >> putStr prompt >> hFlush stdout
                  else
                    D.directCreateMessageHandler D.defaultConfig
            , D.directWaitCreateMessageHandler = False
            , D.directLogger =
                if dumpMsgpack args
                  then putStrLn
                  else D.directLogger D.defaultConfig
            , D.directFormatter =
                if dumpMsgpack args
                  then TL.unpack . pShow
                  else D.directFormatter D.defaultConfig
            }
        pInfo
        ( Hl.runInputT Hl.defaultSettings { Hl.historyFile = Just histFile }
        . startLoop prompt
        )
  where
    argsInfo = Opt.info
        (options <**> Opt.helper)
        (  Opt.fullDesc
        <> Opt.progDesc "Interactive command line client for direct4b.com"
        <> Opt.header ""
        )
    options = Args
        <$> Opt.strArgument (Opt.metavar "LOGIN_INFO_FILE" <> Opt.help "login info file created by \"direct4b login\".")
        <*> Opt.flag True False (Opt.long "no-print-message" <> Opt.short 'q' <> Opt.help "Disable printing received messages")
        <*> Opt.flag False True (Opt.long "dump-msgpack" <> Opt.short 'm' <> Opt.help "Dump all received MsgPack RPC messages")


type State = Maybe D.TalkId

type Prompt = String


consolePrompt :: String -> String
consolePrompt = (++ "@direct4bi> ")


startLoop :: Prompt -> D.Client -> Hl.InputT IO ()
startLoop prompt client = do
    hasT <- Hl.haveTerminalUI
    when hasT
        $ Hl.outputStrLn
              "Connected to direct4b.com. Enter \"help\" to see the available commands"
    loop Nothing prompt client


loop :: State -> Prompt -> D.Client -> Hl.InputT IO ()
loop st prompt client = do
    mcmd <- fmap parseCommand <$> Hl.getInputLine prompt
    case mcmd of
        Just (command, args) -> if command == "quit"
            then return ()
            else do
                newSt <- liftIO $ runCommand st client command args
                loop newSt prompt client
        _ -> return ()


parseCommand :: String -> (String, String)
parseCommand = second (dropWhile isSpace) . break isSpace


runCommand :: State -> D.Client -> String -> String -> IO State
runCommand st client "r" arg = case readMaybe arg of
    Just roomId -> do
        rooms <- D.getTalkRooms client
        if any ((== roomId) . D.talkId) rooms
            then return $ Just roomId
            else do
                hPutStrLn stderr
                    $  "Room#"
                    ++ show roomId
                    ++ " is not your room!"
                return st
    _ -> do
        hPutStrLn stderr $ "Invalid room ID: " ++ show arg
        return st
runCommand st client "p" arg = do
    case st of
        Just roomId -> do
            sendMessageLogging client (D.Txt $ T.pack arg) roomId
        _ -> hPutStrLn stderr noRoomIdConfigured
    return st
runCommand st client "leave" _arg = do
    case st of
        Just roomId -> do
            result <- D.leaveTalkRoom client roomId
            case result of
                Right () ->
                    putStrLn $ "Successfully left room#" ++ show roomId ++ "."
                Left ex -> putStrLn $ "ERROR leaving a room: " ++ show ex

        _ -> hPutStrLn stderr noRoomIdConfigured
    return st
runCommand st client "post" arg = do
    let result = do
            roomId  <- note noRoomIdConfigured st
            selectA <- note ("Invalid Message object: " ++ show arg)
                $ readMaybe arg
            return (roomId, selectA)
    case result of
        Right (roomId, selectA) -> sendMessageLogging client selectA roomId
        Left  emsg              -> hPutStrLn stderr emsg
    return st
runCommand st client "show" "users" = do
    pPrint =<< D.getUsers client
    return st
runCommand st _client "sleep" arg = do
    case readMaybe arg :: Maybe Double of
        Just seconds -> threadDelay $ round $ seconds * 1000 * 1000
        _            -> hPutStrLn stderr $ "Invalid <seconds>: " ++ show arg
    return st
runCommand st _client "help" _arg = do
    putStrLn "r <talk_room_id>: Switch the current talk room by <talk_room_id>"
    putStrLn
        "p <message>: Post <message> as a text message to the current talk room."
    putStrLn "leave: Leave the current talk room."
    putStrLn "post <message>: Post <message> to the current talk room."
    putStrLn "show users: Show the logged-in user and his/her acquaintances."
    putStrLn "sleep <seconds>: Sleep for <seconds> seconds."
    putStrLn "quit: Quit this application."
    putStrLn "help: Print this message."
    return st
runCommand st _client other arg = do
    hPutStrLn stderr $ "Unknown command " ++ show (other ++ arg) ++ "."
    return st


sendMessageLogging :: D.Client -> D.Message -> D.TalkId -> IO ()
sendMessageLogging client msg roomId = do
    result <- D.sendMessage client msg roomId
    case result of
        Right mid ->
            putStrLn $ "Successfully created message#" ++ show mid ++ "."
        Left ex -> putStrLn $ "ERROR creating a message: " ++ show ex


noRoomIdConfigured :: String
noRoomIdConfigured =
    "No room ID configured! Run r <room_id> to configure current room ID!"


printMessage :: (D.Message, D.MessageId, D.TalkRoom, D.User) -> IO ()
printMessage (msg, mid, room, user) = do
    putStrLn
        $  "\nMessage#"
        ++ show mid
        ++ " at room#"
        ++ show (D.talkId room)
        ++ " by user#"
        ++ show (D.userId user)
        ++ ":"
    pPrint msg
