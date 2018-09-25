{-# LANGUAGE OverloadedStrings #-}

module Main
    ( main
    )
where

import           Control.Monad        (void)
import qualified Data.ByteString.Lazy as B
import qualified Data.Text            as T
import qualified System.Signal        as S
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
        body
  where
    body client = do
        S.installHandler S.sigTERM $ \_ ->
            D.shutdown client $ D.Txt "BOTが終了します。\nこの作業は後からやり直してください。"

handleCreateMessage :: D.Client -> (D.Message,D.MessageId,D.TalkRoom,D.User) -> IO ()
handleCreateMessage client (D.Txt txt,_,room,user) | "報告" `T.isInfixOf` txt =
    void $ D.withChannel client (D.pairChannel room user) $ nippo user
handleCreateMessage _client _ = return ()

nippo :: D.User -> D.Channel -> IO ()
nippo peer chan = do
    greeting
    _jobs <- askJobs []
    _eval <- askEvaluation
    bye
  where
    greeting = do
        let msg =
                D.displayName peer
                    `T.append` "さん、\nお疲れ様です。\n今日はどんな業務をしましたか？\n1件1メッセージでお願いします。"
        void $ D.send chan (D.Txt msg)
    askJobs jobs = do
        (msg, _msgid) <- D.recv chan
        case msg of
            D.Txt "。" -> return jobs
            D.Txt job -> do
                void $ D.send
                    chan
                    (D.Txt "他にも業務があれば教えてください。\n(ない場合は、「。」とだけ入力してください)")
                askJobs (job : jobs)
            _ -> do
                void $ D.send chan (D.Txt "終わる場合は、「。」とだけ入力してください)")
                askJobs jobs
    askEvaluation = do
        void $ D.send
            chan
            (D.SelectQ "達成度はどれくらいですか？" ["100", "75", "50", "25", "0"])
        recvLoop
      where
        recvLoop = do
            (msg, _aux) <- D.recv chan
            case msg of
                D.SelectA _ _ ans -> return ans
                _                 -> do
                    void $ D.send chan (D.Txt "数値を選んでください。")
                    recvLoop
    bye = void $ D.send chan (D.Txt "気をつけてお帰りください。")
