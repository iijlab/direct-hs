{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Concurrent     (threadDelay, forkIO)
import           Control.Monad          (forever, void)
import qualified Data.ByteString.Lazy   as B
import           Data.List              (find)
import qualified Data.Text              as T
import qualified Web.Direct             as D

import           Common

main :: IO ()
main = do
    pInfo <- dieWhenLeft . D.deserializePersistedInfo =<< B.readFile jsonFileName
    D.withClient
        D.defaultConfig { D.directLogger               = putStrLn
                        , D.directFormatter            = showMsg
                        , D.directCreateMessageHandler = handleCreateMessage
                        }
        url
        pInfo
        (\_ -> forever $ threadDelay $ 10 * 1000)

handleCreateMessage :: D.Client -> D.Message -> D.Aux -> IO ()
handleCreateMessage client (D.Txt txt) aux
  | "報告" `T.isInfixOf` txt =
    void . forkIO $ D.withChannel client aux $ nippo client aux
handleCreateMessage _client _msg _aux = return ()

nippo :: D.Client -> D.Aux -> D.Channel -> IO ()
nippo client aux0 chan = do
    let peerId = D.auxUserId aux0
    users <- D.getUsers client
    let Just peer = find (\x -> D.userId x == peerId) users
        msg = D.displayName peer `T.append` "さん、\nお疲れ様です。\n今日はどんな業務をしましたか？\n1件1メッセージでお願いします。"
    void $ D.sendMessage client (D.Txt msg) aux0
    loop
  where
    loop = do
        (msg, aux) <- D.recv chan
        case msg of
            D.Txt "。" -> void $ D.sendMessage client (D.Txt "気をつけてお帰りください。") aux
            _          -> do
                void $ D.sendMessage client (D.Txt "他にも業務があれば教えてください。\n(ない場合は、「。」とだけ入力してください)") aux
                loop
