{-# LANGUAGE OverloadedStrings #-}

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
    void $ D.sendMessage client (D.YesNoQ "お元気ですか？") (D.talkId room)
handleCreateMessage client (D.Txt "select", _, room, _) = void $ D.sendMessage
    client
    (D.SelectQ "好きなクワガタは？" ["ヒラタ", "ミヤマ", "オオ"])
    (D.talkId room)
handleCreateMessage client (D.Txt "task", _, room, _) =
    void $ D.sendMessage client (D.TaskQ "高速化できる？" False) (D.talkId room)
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
handleCreateMessage client (D.SelectA "好きなクワガタは？" opt ans, _, room, _) =
    void $ D.sendMessage client
                         (D.Txt (D.getSelectedAnswer opt ans `T.append` "が好きなんですね。"))
                         (D.talkId room)
handleCreateMessage client (D.YesNoA "お元気ですか？" True, _, room, _) =
    void $ D.sendMessage client (D.Txt "よかったです。") (D.talkId room)
handleCreateMessage client (D.YesNoA "お元気ですか？" False, _, room, _) =
    void $ D.sendMessage client (D.Txt "元気出してね。") (D.talkId room)
handleCreateMessage client (D.TaskA "高速化できる？" _ True, _, room, _) =
    void $ D.sendMessage client (D.Txt "よくできました。") (D.talkId room)
handleCreateMessage client (D.TaskA "高速化できる？" _ False, _, room, _) =
    void $ D.sendMessage client (D.Txt "できないんだ。") (D.talkId room)
handleCreateMessage client (D.Location addr _url, _, room, _) =
    void $ D.sendMessage client
                         (D.Txt (addr `T.append` "にいるのね。"))
                         (D.talkId room)
handleCreateMessage client (D.Other txt, _, room, _) = void
    $ D.sendMessage client (D.Txt ("Other" `T.append` txt)) (D.talkId room)
handleCreateMessage _client _ = return ()
