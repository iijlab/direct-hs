{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}

module Web.Direct.CLI.Interactive
    ( defaultMain
    , mainWith
    , noRoomIdConfigured
    , RunCommand
    , State (currentTalk, receivedYesNos, receivedSelects, receivedTasks)
    , defaultRunCommand
    , HelpLine
    , defaultHelpLines
    )
where

import           Control.Applicative        (optional, (<**>))
import           Control.Arrow              (second)
import           Control.Concurrent         (threadDelay)
import           Control.Error              (failWith, failWithM, note)
import           Control.Monad              (when, (<=<))
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Except (runExceptT, throwE)
import qualified Data.ByteString.Lazy       as B
import           Data.Char                  (isSpace)
import           Data.Foldable              (for_)
import qualified Data.IntMap.Strict         as IM
import qualified Data.IORef                 as IOR
import           Data.List                  (sort)
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Options.Applicative        as Opt
import qualified System.Console.Haskeline   as Hl
import           System.Exit                (die)
import qualified System.FilePath            as FP
import           System.IO                  (hFlush, hPutStrLn, stderr, stdout)
import           Text.Pretty.Simple         (pPrint, pShow)
import           Text.Read                  (readMaybe)

#ifdef mingw32_HOST_OS
import           GHC.IO.Encoding.CodePage   (mkLocaleEncoding)
import           GHC.IO.Encoding.Failure    (CodingFailureMode (TransliterateCodingFailure))
import           System.IO                  (hSetEncoding)
#endif

import qualified Web.Direct                 as D


data Args = Args
    { loginInfoFile :: FilePath
    , logMessage    :: Bool
    , dumpMsgpack   :: Bool
    , initialTalkId :: Maybe D.TalkId
    } deriving (Eq, Show)


type RunCommand = State -> D.Client -> String -> String -> IO State


data State = State {
      currentTalk     :: !(Maybe D.TalkId)
    , receivedYesNos  :: !(IOR.IORef (IM.IntMap D.YesNoQuestion))
    , receivedSelects :: !(IOR.IORef (IM.IntMap D.SelectQuestion))
    , receivedTasks   :: !(IOR.IORef (IM.IntMap D.TaskQuestion))
    }


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
    let getPrompt =
            if hasT
              then fmap ((++ "@direct4bi> ") . T.unpack . D.displayName) . D.getMe
              else const (return "")

    st <- mkInitialState $ initialTalkId args

    let loggingHandler =
            if logMessage args
              then \client msg -> do
                  printMessage msg
                  putStr =<< getPrompt client
                  hFlush stdout
              else D.directCreateMessageHandler D.defaultConfig

    D.withClient
        D.defaultConfig
            { D.directCreateMessageHandler     = \client msg -> do
                loggingHandler client msg
                questionHandler st msg
            , D.directWaitCreateMessageHandler = False
            , D.directLogger                   = if dumpMsgpack args
                                                     then putStrLn
                                                     else D.directLogger D.defaultConfig
            , D.directFormatter                = if dumpMsgpack args
                                                     then TL.unpack . pShow
                                                     else D.directFormatter D.defaultConfig
            }
        pInfo
        $ \client -> do
            prompt <- getPrompt client
            Hl.runInputT Hl.defaultSettings { Hl.historyFile = Just histFile }
                $ startLoop runCommand st prompt client
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
            <*> (optional
                    (Opt.option
                        Opt.auto
                        (Opt.metavar "TALK_ID" <> Opt.long "talk-room" <> Opt.short 't' <> Opt.help
                            "Initial talk room ID"
                        )
                    )
                )


defaultMain :: IO ()
defaultMain = mainWith $ defaultRunCommand defaultHelpLines


type Prompt = String


startLoop :: RunCommand -> State -> Prompt -> D.Client -> Hl.InputT IO ()
startLoop runCommand initial prompt client = do
    hasT <- Hl.haveTerminalUI
    when hasT
        $ Hl.outputStrLn
              "Connected to direct4b.com. Enter \"help\" to see the available commands"
    loop runCommand initial prompt client


loop :: RunCommand -> State -> Prompt -> D.Client -> Hl.InputT IO ()
loop runCommand st prompt client = do
    mcmd <- fmap parseCommand <$> Hl.getInputLine prompt
    case mcmd of
        Just (command, args) | command /= "q" -> do
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
    , "a <question_id> <yes/no, option_number, done>: Send answer for the message."
    , "leave: Leave the current talk room."
    , "post <message>: Post <message> to the current talk room."
    , "show users: Show the logged-in user and his/her acquaintances."
    , "show rooms: Show the logged-in user's talk rooms and their participants."
    , "show yesnos: Show received Yes/No question messages with question_id."
    , "show selects: Show received Select question message with question_ids."
    , "show tasks: Show received Task messages with question_id."
    , "sleep <seconds>: Sleep for <seconds> seconds."
    , "q: Quit this application."
    , "help: Print this message."
    ]


defaultRunCommand :: [HelpLine] -> RunCommand
defaultRunCommand _hs st client "r" arg = case readMaybe arg of
    Just roomId -> do
        rooms <- D.getTalkRooms client
        if any ((== roomId) . D.talkId) rooms
            then return $ st { currentTalk = Just roomId }
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
    case currentTalk st of
        Just roomId -> sendMessageLogging client (D.Txt $ T.pack arg) roomId
        Nothing     -> hPutStrLn stderr noRoomIdConfigured
    return st
defaultRunCommand _hs st client "a" arg = do
    either (hPutStrLn stderr) return <=< runExceptT $ do
        roomId <- failWith noRoomIdConfigured $ currentTalk st
        (qid, ans) <- parseAnswer
        case ans of
            "yes"  -> do
                D.YesNoQuestion { D.question, D.closingType } <- getYesNoQ qid
                let msg = D.YesNoA D.YesNoAnswer
                        { D.question = question
                        , D.closingType = closingType
                        , D.response = True
                        , D.inReplyTo = fromIntegral qid
                        }
                liftIO $ sendMessageLogging client msg roomId
            "no"   -> do
                D.YesNoQuestion { D.question, D.closingType } <- getYesNoQ qid
                let msg = D.YesNoA D.YesNoAnswer
                        { D.question = question
                        , D.closingType = closingType
                        , D.response = False
                        , D.inReplyTo = fromIntegral qid
                        }
                liftIO $ sendMessageLogging client msg roomId
            "done" -> do
                D.TaskQuestion { D.title, D.closingType } <-
                    failWithM "No such task found!" (IM.lookup qid <$> IOR.readIORef (receivedTasks st))
                let msg = D.TaskA D.TaskAnswer
                        { D.title = title
                        , D.closingType = closingType
                        , D.done = True
                        , D.inReplyTo = fromIntegral qid
                        }
                liftIO $ sendMessageLogging client msg roomId
            _other -> do
                n <- failWith ("Invalid answer: " ++ show ans) $ readMaybe ans
                D.SelectQuestion { D.question, D.options, D.closingType } <-
                    failWithM "No such select question found!" (IM.lookup qid <$> IOR.readIORef (receivedSelects st))
                let optCount = length options
                if baseSelectAnswerNumber <= n && n <= optCount
                  then do
                    let msg = D.SelectA D.SelectAnswer
                            { D.question = question
                            , D.closingType = closingType
                            , D.options = options
                            , D.response = n - baseSelectAnswerNumber
                            , D.inReplyTo = fromIntegral qid
                            }
                    liftIO $ sendMessageLogging client msg roomId
                  else
                    throwE $ "Invalid answer number (from " ++ show baseSelectAnswerNumber ++ " to " ++ show optCount ++ ")"
    return st
  where
    parseAnswer =
        case words arg of
            qidS : ans : _ -> do
                qid <- failWith "Invalid question ID" (readMaybe qidS)
                return (qid, ans)
            _ ->
                throwE "Invalid answer"
    getYesNoQ qid =
        failWithM "No such yes/no question found!" (IM.lookup qid <$> IOR.readIORef (receivedYesNos st))
defaultRunCommand _hs st client "leave" _arg = do
    case currentTalk st of
        Just roomId -> do
            result <- D.leaveTalkRoom client roomId
            case result of
                Right () ->
                    putStrLn $ "Successfully left room#" ++ show roomId ++ "."
                Left ex -> hPutStrLn stderr $ "ERROR leaving a room: " ++ show ex

        Nothing -> hPutStrLn stderr noRoomIdConfigured
    return st
defaultRunCommand _hs st client "post" arg = do
    let result = do
            roomId  <- note noRoomIdConfigured $ currentTalk st
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

defaultRunCommand _hs st client "show" "yesnos"  = do
    ryns <- IOR.readIORef $ receivedYesNos st
    for_ (IM.toAscList ryns) $ \(qid, D.YesNoQuestion { D.question }) ->
        putStrLn $ show qid ++ ": " ++ "\"" ++ T.unpack question ++ "\""
    return st

defaultRunCommand _hs st client "show" "selects" = do
    ss <- IOR.readIORef $ receivedSelects st
    for_ (IM.toAscList ss) $ \(qid, D.SelectQuestion { D.question, D.options }) -> do
        putStrLn $ show qid ++ ": " ++ "\"" ++ T.unpack question ++ "\""
        for_ (zip [baseSelectAnswerNumber..] options) $ \(n, option) ->
            putStrLn $ "  " ++ show n ++ ": " ++ "\"" ++ T.unpack option ++ "\""
    return st
defaultRunCommand _hs st client "show" "tasks"   = do
    rts <- IOR.readIORef $ receivedTasks st
    for_ (IM.toAscList rts) $ \(qid, D.TaskQuestion { D.title }) ->
        putStrLn $ show qid ++ ": " ++ "\"" ++ T.unpack title ++ "\""
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


questionHandler :: State -> (D.Message, D.MessageId, D.TalkRoom, D.User) -> IO ()
questionHandler st (msg, mid, _, _) =
    case msg of
        D.YesNoQ ynq ->
            IOR.atomicModifyIORef' (receivedYesNos st) $ \mp -> (IM.insert (fromIntegral mid) ynq mp, ())
        D.SelectQ sq ->
            IOR.atomicModifyIORef' (receivedSelects st) $ \mp -> (IM.insert (fromIntegral mid) sq mp, ())
        D.TaskQ tq ->
            IOR.atomicModifyIORef' (receivedTasks st) $ \mp -> (IM.insert (fromIntegral mid) tq mp, ())
        _ ->
            return ()


baseSelectAnswerNumber :: Int
baseSelectAnswerNumber = 1


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

mkInitialState :: Maybe D.TalkId -> IO State
mkInitialState mtid = State mtid
    <$> IOR.newIORef mempty
    <*> IOR.newIORef mempty
    <*> IOR.newIORef mempty


avoidCodingError :: IO ()
#ifdef mingw32_HOST_OS
avoidCodingError =
  hSetEncoding stdout $ mkLocaleEncoding TransliterateCodingFailure
#else
avoidCodingError = return ()
#endif
