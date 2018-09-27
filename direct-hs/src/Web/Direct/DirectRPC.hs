{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.DirectRPC where

import qualified Data.MessagePack                         as M
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.Exception
import           Web.Direct.Message
import           Web.Direct.Types

createMessage :: RPC.Client -> Message -> TalkId -> IO (Either Exception MessageId)
createMessage rpcclient req tid = do
    let obj        = encodeMessage req tid
        methodName = "create_message"
    ersp <-
        resultToObjectOrException methodName
            <$> RPC.call rpcclient methodName obj
    case ersp of
        Right rsp@(M.ObjectMap rspMap) ->
            case lookup (M.ObjectStr "message_id") rspMap of
                Just (M.ObjectWord x) -> return $ Right x
                _ -> return $ Left $ UnexpectedReponse methodName rsp
        Right other -> return $ Left $ UnexpectedReponse methodName other
        Left  other -> return $ Left other


