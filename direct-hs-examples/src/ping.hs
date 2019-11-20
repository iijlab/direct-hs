{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main
    ( main
    )
where

import           Control.Monad        (void)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified Web.Direct           as D

import           Common

main :: IO ()
main = do
    pInfo <- dieWhenLeft . D.deserializeLoginInfo =<< B.readFile jsonFileName
    D.withClient
        D.defaultConfig { D.directLogger               = putStrLn
                        , D.directCreateMessageHandler = handleCreateMessage
                        }
        pInfo
        (\_ -> return ())

handleCreateMessage
    :: D.Client -> (D.Message, D.MessageId, D.TalkRoom, D.User) -> IO ()
handleCreateMessage client (D.Txt "ping", _, room, _) =
    void $ D.sendMessage client (D.Txt "pong") (D.talkId room)
handleCreateMessage client (D.Txt "stamp", _, room, _) = void $ D.sendMessage
    client
    (D.Stamp 3 1152921507291204297 (Just "スタンプ！"))
    (D.talkId room)
handleCreateMessage client (D.Txt "yesno", _, room, _) =
    void $ D.sendMessage client (D.YesNoQ $ D.YesNoQuestion "お元気ですか？" D.Anyone) (D.talkId room)
handleCreateMessage client (D.Txt "select", _, room, _) = void $ D.sendMessage
    client
    (D.SelectQ $ D.SelectQuestion "好きなクワガタは？" ["ヒラタ", "ミヤマ", "オオ"] D.Anyone)
    (D.talkId room)
handleCreateMessage client (D.Txt "task", _, room, _) =
    void $ D.sendMessage client (D.TaskQ $ D.TaskQuestion "高速化できる？" D.Anyone) (D.talkId room)
handleCreateMessage client (D.Txt "whoareyou", _, room, _) = do
    me <- D.getMe client
    void $ D.sendMessage
        client
        (D.Txt $ "私は" `T.append` D.displayName me `T.append` "です")
        (D.talkId room)
handleCreateMessage client (D.Txt "users", _, room, _) = do
    users <- D.getUsers client
    let ans = T.unlines $ map D.displayName users
    void $ D.sendMessage client (D.Txt (ans `T.append` "がいます。")) (D.talkId room)
handleCreateMessage client (D.Txt "domains", _, room, _) = do
    doms <- D.getDomains client
    let ans = T.unlines $ map D.domainName doms
    void $ D.sendMessage client
                         (D.Txt (ans `T.append` "があります。"))
                         (D.talkId room)
handleCreateMessage client (D.Txt "talks", _, room, _) = do
    talks <- D.getTalkRooms client
    let ans = T.unlines $ map (T.pack . show . D.talkType) talks
    void $ D.sendMessage client
                         (D.Txt (ans `T.append` "があります。"))
                         (D.talkId room)
handleCreateMessage client (D.SelectA sa@D.SelectAnswer { D.question = "好きなクワガタは？" }, _, room, _) =
    void $ D.sendMessage client
                         (D.Txt (D.getSelectedAnswer sa `T.append` "が好きなんですね。"))
                         (D.talkId room)
handleCreateMessage client (D.YesNoA D.YesNoAnswer { D.question = "お元気ですか？", D.response = True }, _, room, _) =
    void $ D.sendMessage client (D.Txt "よかったです。") (D.talkId room)
handleCreateMessage client (D.YesNoA D.YesNoAnswer { D.question = "お元気ですか？", D.response = False }, _, room, _) =
    void $ D.sendMessage client (D.Txt "元気出してね。") (D.talkId room)
handleCreateMessage client (D.TaskA D.TaskAnswer { D.title = "高速化できる？", D.done = True }, _, room, _) =
    void $ D.sendMessage client (D.Txt "よくできました。") (D.talkId room)
handleCreateMessage client (D.TaskA D.TaskAnswer { D.title = "高速化できる？", D.done = False }, _, room, _) =
    void $ D.sendMessage client (D.Txt "できないんだ。") (D.talkId room)
handleCreateMessage client (D.Location addr _url, _, room, _) =
    void $ D.sendMessage client
                         (D.Txt (addr `T.append` "にいるのね。"))
                         (D.talkId room)
handleCreateMessage client (D.Other txt, _, room, _) = void
    $ D.sendMessage client (D.Txt ("Other" `T.append` txt)) (D.talkId room)
handleCreateMessage _client _ = return ()
