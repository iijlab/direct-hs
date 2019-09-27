{-# LANGUAGE CPP #-}

module Web.Direct.CLI.Interactive
    ( defaultMain
    , mainWith
    , noRoomIdConfigured
    , RunCommand
    , State
    , defaultRunCommand
    , HelpLine
    , defaultHelpLines
    )
where

import           Control.Applicative      ((<**>))
import           Control.Arrow            (second)
import           Control.Concurrent       (threadDelay)
import           Control.Error            (note)
import           Control.Monad            (when)
import           Control.Monad.IO.Class   (liftIO)
import qualified Data.ByteString.Lazy     as B
import           Data.Char                (isSpace)
import           Data.Foldable            (for_)
import           Data.List                (break, sort)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL
import qualified Options.Applicative      as Opt
import qualified System.Console.Haskeline as Hl
import           System.Exit              (die)
import qualified System.FilePath          as FP
import           System.IO                (hFlush, hPutStrLn, stderr, stdout)
import           Text.Pretty.Simple       (pPrint, pShow)
import           Text.Read                (readMaybe)

#ifdef mingw32_HOST_OS
import           GHC.IO.Encoding.CodePage (mkLocaleEncoding)
import           GHC.IO.Encoding.Failure  (CodingFailureMode (TransliterateCodingFailure))
import           System.IO                (hSetEncoding)
#endif

import qualified Web.Direct               as D


data Args = Args
    { loginInfoFile :: FilePath
    , logMessage    :: Bool
    , dumpMsgpack   :: Bool
    } deriving (Eq, Show)


type RunCommand = State -> D.Client -> String -> String -> IO State


type State = Maybe D.TalkId


data TalkRoomAndParticipants = TalkRoomAndParticipants
    { talkRoom         :: D.TalkRoom
    , talkParticipants :: [D.User]
    } deriving (Eq, Show)


mainWith :: RunCommand -> IO ()
mainWith runCommand = do
    avoidCodingError

    args <- Opt.execParser argsInfo

    let jsonBaseName = FP.takeBaseName $ loginInfoFile args
        histFile     = jsonBaseName ++ ".history.txt"
    pInfo <- dieWhenLeft . D.deserializeLoginInfo =<< B.readFile
        (loginInfoFile args)
    hasT <- Hl.runInputT Hl.defaultSettings Hl.haveTerminalUI
    let prompt = if hasT then consolePrompt jsonBaseName else ""

    D.withClient
        D.defaultConfig
            { D.directCreateMessageHandler     =
                if logMessage args
                    then \_client msg ->
                        printMessage msg >> putStr prompt >> hFlush stdout
                    else D.directCreateMessageHandler D.defaultConfig
            , D.directWaitCreateMessageHandler = False
            , D.directLogger                   = if dumpMsgpack args
                                                     then putStrLn
                                                     else D.directLogger D.defaultConfig
            , D.directFormatter                = if dumpMsgpack args
                                                     then TL.unpack . pShow
                                                     else D.directFormatter D.defaultConfig
            }
        pInfo
        ( Hl.runInputT Hl.defaultSettings { Hl.historyFile = Just histFile }
        . startLoop runCommand prompt
        )
  where
    argsInfo = Opt.info
        (options <**> Opt.helper)
        (  Opt.fullDesc
        <> Opt.progDesc "Interactive command line client for direct4b.com"
        <> Opt.header ""
        )
    options =
        Args
            <$> Opt.strArgument
                    (Opt.metavar "LOGIN_INFO_FILE" <> Opt.help
                        "login info file created by \"direct4b login\"."
                    )
            <*> Opt.flag
                    True
                    False
                    (Opt.long "no-print-message" <> Opt.short 'q' <> Opt.help
                        "Disable printing received messages"
                    )
            <*> Opt.flag
                    False
                    True
                    (Opt.long "dump-msgpack" <> Opt.short 'm' <> Opt.help
                        "Dump all received MsgPack RPC messages"
                    )


defaultMain :: IO ()
defaultMain = mainWith $ defaultRunCommand defaultHelpLines

type Prompt = String

consolePrompt :: String -> String
consolePrompt = (++ "@direct4bi> ")


startLoop :: RunCommand -> Prompt -> D.Client -> Hl.InputT IO ()
startLoop runCommand prompt client = do
    hasT <- Hl.haveTerminalUI
    when hasT
        $ Hl.outputStrLn
              "Connected to direct4b.com. Enter \"help\" to see the available commands"
    loop runCommand Nothing prompt client


loop :: RunCommand -> State -> Prompt -> D.Client -> Hl.InputT IO ()
loop runCommand st prompt client = do
    mcmd <- fmap parseCommand <$> Hl.getInputLine prompt
    case mcmd of
        Just (command, args) -> if command == "quit"
            then return ()
            else do
                newSt <- liftIO $ runCommand st client command args
                loop runCommand newSt prompt client
        _ -> return ()


parseCommand :: String -> (String, String)
parseCommand = second (dropWhile isSpace) . break isSpace . dropWhile isSpace

type HelpLine = String

defaultHelpLines :: [HelpLine]
defaultHelpLines =
    [ "r <talk_room_id>: Switch the current talk room by <talk_room_id>"
    , "p <message>: Post <message> as a text message to the current talk room."
    , "leave: Leave the current talk room."
    , "post <message>: Post <message> to the current talk room."
    , "show users: Show the logged-in user and his/her acquaintances."
    , "show rooms: Show the logged-in user's talk rooms and their participants."
    , "sleep <seconds>: Sleep for <seconds> seconds."
    , "quit: Quit this application."
    , "help: Print this message."
    ]


defaultRunCommand :: [HelpLine] -> RunCommand
defaultRunCommand _hs st client "r" arg = case readMaybe arg of
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
defaultRunCommand _hs st client "p" arg = do
    case st of
        Just roomId -> sendMessageLogging client (D.Txt $ T.pack arg) roomId
        _           -> hPutStrLn stderr noRoomIdConfigured
    return st
defaultRunCommand _hs st client "leave" _arg = do
    case st of
        Just roomId -> do
            result <- D.leaveTalkRoom client roomId
            case result of
                Right () ->
                    putStrLn $ "Successfully left room#" ++ show roomId ++ "."
                Left ex -> hPutStrLn stderr $ "ERROR leaving a room: " ++ show ex

        _ -> hPutStrLn stderr noRoomIdConfigured
    return st
defaultRunCommand _hs st client "post" arg = do
    let result = do
            roomId  <- note noRoomIdConfigured st
            selectA <- note ("Invalid Message object: " ++ show arg)
                $ readMaybe arg
            return (roomId, selectA)
    case result of
        Right (roomId, selectA) -> sendMessageLogging client selectA roomId
        Left  emsg              -> hPutStrLn stderr emsg
    return st
defaultRunCommand _hs st client "show" "users" = do
    pPrint =<< D.getUsers client
    return st
defaultRunCommand _hs st client "show" "rooms" = do
    talks <- D.getTalkRooms client
    for_ talks $ \talk -> do
        talkUsers <- D.getTalkUsers client talk
        pPrint $ TalkRoomAndParticipants talk talkUsers

    return st
defaultRunCommand _hs st _client "sleep" arg = do
    case readMaybe arg :: Maybe Double of
        Just seconds -> threadDelay $ round $ seconds * 1000 * 1000
        _            -> hPutStrLn stderr $ "Invalid <seconds>: " ++ show arg
    return st
defaultRunCommand hs st _client "help" _arg = do
    for_ (sort hs) putStrLn
    return st
defaultRunCommand _hs st _client other arg = do
    hPutStrLn stderr $ "Unknown command " ++ show (other ++ " " ++ arg) ++ "."
    return st


sendMessageLogging :: D.Client -> D.Message -> D.TalkId -> IO ()
sendMessageLogging client msg roomId = do
    result <- D.sendMessage client msg roomId
    case result of
        Right mid ->
            putStrLn $ "Successfully created message#" ++ show mid ++ "."
        Left ex -> hPutStrLn stderr $ "ERROR creating a message: " ++ show ex


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

dieWhenLeft :: Either String a -> IO a
dieWhenLeft = either exitError return

exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"


avoidCodingError :: IO ()
#ifdef mingw32_HOST_OS
avoidCodingError =
  hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
#else
avoidCodingError = return ()
#endif
