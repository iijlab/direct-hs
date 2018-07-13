{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, void)
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text              as T
import qualified Web.Direct             as D

import           Common

main :: IO ()
main = do
    pInfo <- dieWhenLeft . D.deserializePersistedInfo =<< B.readFile jsonFileName
    D.withClient
        D.defaultConfig { D.directLogger               = putStrLn
                        , D.directCreateMessageHandler = handleCreateMessage
                        }
        url
        pInfo
        (\_ -> forever $ threadDelay $ 10 * 1000)

handleCreateMessage :: D.Client -> D.Message -> D.Aux -> IO ()
handleCreateMessage client (D.Txt "ping") aux =
    void $ D.sendMessage client (D.Txt "pong") aux
handleCreateMessage client (D.Txt "stamp") aux =
    void $ D.sendMessage client (D.Stamp 3 1152921507291204297) aux
handleCreateMessage client (D.Txt "yesno") aux =
    void $ D.sendMessage client (D.YesNoQ "お元気ですか？") aux
handleCreateMessage client (D.Txt "select") aux =
    void $ D.sendMessage client (D.SelectQ "好きなクワガタは？" ["ヒラタ", "ミヤマ", "オオ"]) aux
handleCreateMessage client (D.Txt "task") aux =
    void $ D.sendMessage client (D.TaskQ "高速化できる？" False) aux
handleCreateMessage client (D.Txt "whoareyou") aux = do
    Just me <- D.getMe client
    void $ D.sendMessage client (D.Txt $ "私は" `T.append` D.displayName me `T.append` "です") aux
handleCreateMessage client (D.Txt "users") aux = do
    users <- D.getUsers client
    let ans = T.unlines $ map D.displayName users
    void $ D.sendMessage client (D.Txt (ans `T.append` "がいます。")) aux
handleCreateMessage client (D.Txt "domains") aux = do
    doms <- D.getDomains client
    let ans = T.unlines $ map D.domainName doms
    void $ D.sendMessage client (D.Txt (ans `T.append` "があります。")) aux
handleCreateMessage client (D.Txt "talks") aux = do
    talks <- D.getTalkRooms client
    let ans = T.unlines $ map (T.pack . show . D.talkType) talks
    void $ D.sendMessage client (D.Txt (ans `T.append` "があります。")) aux
handleCreateMessage client (D.SelectA "好きなクワガタは？" _ ans) aux =
    void $ D.sendMessage client (D.Txt (ans `T.append` "が好きなんですね。")) aux
handleCreateMessage client (D.YesNoA "お元気ですか？" True) aux =
    void $ D.sendMessage client (D.Txt "よかったです。") aux
handleCreateMessage client (D.YesNoA "お元気ですか？" False) aux =
    void $ D.sendMessage client (D.Txt "元気出してね。") aux
handleCreateMessage client (D.TaskA "高速化できる？" _ True) aux =
    void $ D.sendMessage client (D.Txt "よくできました。") aux
handleCreateMessage client (D.TaskA "高速化できる？" _ False) aux =
    void $ D.sendMessage client (D.Txt "できないんだ。") aux
handleCreateMessage client (D.Location addr _url) aux =
    void $ D.sendMessage client (D.Txt (addr `T.append` "にいるのね。")) aux
handleCreateMessage client (D.Other txt) aux =
    void $ D.sendMessage client (D.Txt ("Other" `T.append` txt)) aux
handleCreateMessage _client _msg _aux = return ()
