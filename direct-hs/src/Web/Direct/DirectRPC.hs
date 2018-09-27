{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.DirectRPC where

import qualified Data.MessagePack                         as M
import           Data.Text (Text)
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Exception
import           Web.Direct.LoginInfo
import           Web.Direct.Message
import           Web.Direct.Types

createMessage :: RPC.Client -> Message -> TalkId -> IO (Either Exception MessageId)
createMessage rpcclient req tid = do
    let methodName = "create_message"
        obj        = encodeMessage req tid
    rsp <- RPC.call rpcclient methodName obj
    case resultToObjectOrException methodName rsp of
        Right rs@(M.ObjectMap rspMap) ->
            case lookup (M.ObjectStr "message_id") rspMap of
                Just (M.ObjectWord x) -> return $ Right x
                _ -> return $ Left $ UnexpectedReponse methodName rs
        Right other -> return $ Left $ UnexpectedReponse methodName other
        Left  other -> return $ Left other

createAccessToken :: RPC.Client -> Text -> Text -> Text -> Text -> IO (Either Exception LoginInfo)
createAccessToken rpcclient email pass idfv agentName = do
    let methodName    = "create_access_token"
        magicConstant = M.ObjectStr ""
    rsp <- RPC.call
        rpcclient
        methodName
        [ M.ObjectStr email
        , M.ObjectStr pass
        , M.ObjectStr idfv
        , M.ObjectStr agentName
        , magicConstant
        ]
    case resultToObjectOrException methodName rsp of
       Right (M.ObjectStr token) -> return $ Right $ LoginInfo token idfv
       Right other -> return $ Left $ UnexpectedReponse methodName other
       Left e -> return $ Left e
