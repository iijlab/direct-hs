{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.DirectRPC where

import           Control.Monad                            (void)
import qualified Data.MessagePack                         as M
import           Data.Text                                (Text)
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC

import           Web.Direct.DirectRPC.Map
import           Web.Direct.Exception
import           Web.Direct.LoginInfo
import           Web.Direct.Message
import           Web.Direct.Types

createMessage
    :: RPC.Client -> Message -> TalkId -> IO (Either Exception MessageId)
createMessage rpcclient req tid = do
    let methodName = "create_message"
        obj        = encodeMessage req tid
    eres <- callRpc rpcclient methodName obj
    case eres of
        Right rs@(M.ObjectMap rspMap) ->
            case lookup (M.ObjectStr "message_id") rspMap of
                Just (M.ObjectWord x) -> return $ Right x
                _ -> return $ Left $ UnexpectedReponse methodName rs
        Right other -> return $ Left $ UnexpectedReponse methodName other
        Left  e     -> return $ Left e

createAccessToken
    :: RPC.Client
    -> Text
    -> Text
    -> Text
    -> Text
    -> IO (Either Exception LoginInfo)
createAccessToken rpcclient email pass idfv agntnm = do
    let methodName = "create_access_token"
        obj =
            [ M.ObjectStr email
            , M.ObjectStr pass
            , M.ObjectStr idfv
            , M.ObjectStr agntnm
            , magicConstant
            ]
        magicConstant = M.ObjectStr ""
    eres <- callRpc rpcclient methodName obj
    case eres of
        Right (M.ObjectStr token) -> return $ Right $ LoginInfo token idfv
        Right other -> return $ Left $ UnexpectedReponse methodName other
        Left e -> return $ Left e

agentName :: Text
agentName = "bot"

apiVersion :: Text
apiVersion = "1.91"

createSession :: RPC.Client -> Text -> IO User
createSession rpcclient info = do
    let methodName = "create_session"
        obj = [M.ObjectStr info, M.ObjectStr apiVersion, M.ObjectStr agentName]
    rsp <- callRpcThrow rpcclient methodName obj
    convertOrThrow methodName fromCreateSession rsp

resetNotification :: RPC.Client -> IO ()
resetNotification rpcclient =
    void $ callRpcThrow rpcclient "reset_notification" []

startNotification :: RPC.Client -> IO ()
startNotification rpcclient =
    void $ callRpcThrow rpcclient "start_notification" []

getDomains :: RPC.Client -> IO [Domain]
getDomains rpcclient =
    fromGetDomains <$> callRpcThrow rpcclient "get_domains" []

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
getFriends rpcclient = void $ callRpcThrow rpcclient "get_friends" []

getAcquaintances :: RPC.Client -> IO [User]
getAcquaintances rpcclient =
    fromGetAcquaintances <$> callRpcThrow rpcclient "get_acquaintances" []

getTalks :: RPC.Client -> [User] -> IO [TalkRoom]
getTalks rpcclient users =
    fromGetTalks users <$> callRpcThrow rpcclient "get_talks" []

getTalkStatuses :: RPC.Client -> IO ()
getTalkStatuses rpcclient =
    void $ callRpcThrow rpcclient "get_talk_statuses" []

createPairTalk :: RPC.Client -> User -> IO TalkRoom
createPairTalk rpcclient peer = do
    dom : _ <- getDomains rpcclient
    let methodName = "create_pair_talk"
        did        = domainId dom
        uid        = userId peer
        dat        = [M.ObjectWord did, M.ObjectWord uid]
    rsp <- callRpcThrow rpcclient methodName dat
    convertOrThrow methodName (decodeTalkRoom [peer]) rsp -- fixme: users
