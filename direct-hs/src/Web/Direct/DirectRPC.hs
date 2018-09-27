{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.DirectRPC where

import           Control.Monad                            (void)
import qualified Data.MessagePack                         as M
import           Data.Text                                (Text)
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

resetNotification :: RPC.Client -> IO ()
resetNotification rpcclient =
    void $ callRpcThrow rpcclient "reset_notification" []

startNotification :: RPC.Client -> IO ()
startNotification rpcclient =
    void $ callRpcThrow rpcclient "start_notification" []

getDomains :: RPC.Client -> IO M.Object
getDomains rpcclient =
    callRpcThrow rpcclient "get_domains" []

getDomainInvites :: RPC.Client -> IO ()
getDomainInvites rpcclient =
    void $ callRpcThrow rpcclient "get_domain_invites" []

getAccountControlRequests :: RPC.Client -> IO ()
getAccountControlRequests rpcclient =
    void $ callRpcThrow rpcclient "get_account_control_requests" []

getJoinedAccountControlGroup :: RPC.Client -> IO ()
getJoinedAccountControlGroup rpcclient =
    void $ callRpcThrow rpcclient "get_joined_account_control_group" []

getAnnouncementStatuses :: RPC.Client -> IO ()
getAnnouncementStatuses rpcclient =
    void $ callRpcThrow rpcclient "get_announcement_statuses" []

getFriends :: RPC.Client -> IO ()
getFriends rpcclient =
    void $ callRpcThrow rpcclient "get_friends" []

getAcquaintances :: RPC.Client -> IO M.Object
getAcquaintances rpcclient = callRpcThrow rpcclient "get_acquaintances" []

getTalks :: RPC.Client -> IO M.Object
getTalks rpcclient = callRpcThrow rpcclient "get_talks" []

getTalkStatuses :: RPC.Client -> IO ()
getTalkStatuses rpcclient =
    void $ callRpcThrow rpcclient "get_talk_statuses" []
