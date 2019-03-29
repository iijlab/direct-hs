{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.DirectRPC where

import           Control.Monad                            (void)
import qualified Data.MessagePack                         as M
import qualified Data.MessagePack.RPC                     as R
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

getAcquaintances :: RPC.Client -> IO [(DomainId, [User])]
getAcquaintances rpcclient =
    fromGetAcquaintances <$> callRpcThrow rpcclient "get_acquaintances" []

getTalks :: RPC.Client -> IO [(DomainId, [TalkRoom])]
getTalks rpcclient = fromGetTalks <$> callRpcThrow rpcclient "get_talks" []

getTalkStatuses :: RPC.Client -> IO ()
getTalkStatuses rpcclient =
    void $ callRpcThrow rpcclient "get_talk_statuses" []

deleteTalker :: RPC.Client -> TalkRoom -> User -> IO (Either Exception ())
deleteTalker rpcclient talk user = do
    let methodName = "delete_talker"
        tid        = talkId talk
        uid        = userId user
        dat        = [M.ObjectWord tid, M.ObjectWord uid]
    void <$> callRpc rpcclient methodName dat

createUploadAuth
    :: RPC.Client
    -> Text
    -> Text
    -> FileSize
    -> Domain
    -> IO (Either Exception UploadAuth)
createUploadAuth rpcclient fn mimeType fileSize dom = do
    let methodName = "create_upload_auth"
        obj =
            [ M.ObjectStr fn
            , M.ObjectStr mimeType
            , M.ObjectWord fileSize
            , M.ObjectWord $ domainId dom
            ]
    eres <- callRpc rpcclient methodName obj
    case eres of
        Right rsp@(M.ObjectMap rspMap) -> case decodeUploadAuth rspMap of
            Just ua -> return $ Right ua
            _       -> return $ Left $ UnexpectedReponse methodName rsp
        Right other -> return $ Left $ UnexpectedReponse methodName other
        Left  e     -> return $ Left e

----------------------------------------------------------------

data NotificationHandlers = NotificationHandlers
    { onNotifyCreateMessage :: Message -> MessageId -> TalkId -> UserId -> IO ()
    , onNotifyDeleteTalk :: TalkId -> IO ()
    , onNotifyDeleteTalker :: DomainId -> TalkId -> [UserId] -> [UserId] -> IO ()
    }

handleNotification
    :: R.MethodName -> [M.Object] -> NotificationHandlers -> IO ()
handleNotification method params handlers = case (method, params) of
    ("notify_create_message", M.ObjectMap rsp : _) -> case decodeMessage rsp of
        Just (msg, msgid, tid, uid) ->
            onNotifyCreateMessage handlers msg msgid tid uid
        _ -> return ()
    ("notify_delete_talk", M.ObjectWord tid : _) ->
        onNotifyDeleteTalk handlers tid
    ("notify_delete_talker", obj : _) -> case decodeTalker obj of
        Just (did, tid, uids, leftUids) ->
            onNotifyDeleteTalker handlers did tid uids leftUids
        _ -> return ()
    _ -> return ()
