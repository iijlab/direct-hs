{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Exception
    ( Exception(..)
    , callRpc
    , callRpcThrow
    , convertOrThrow
    )
where


import           Control.Error                            (fmapL)
import qualified Control.Exception                        as E
import qualified Data.MessagePack                         as M
import qualified Data.MessagePack.RPC                     as RPC
import           Data.Typeable                            (Typeable)
import           Network.HTTP.Types.Status                (Status)
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC


data Exception =
      InvalidEmailOrPassword
    | InvalidTalkId
    | InvalidTalkType
    | InvalidUserId
    | InvalidWsUrl !String
    | UnexpectedReponseWhenUpload Status
    | UnexpectedReponse !RPC.MethodName !M.Object
  deriving (Eq, Show, Typeable)

instance E.Exception Exception

fromErrorObject :: RPC.MethodName -> M.Object -> Exception
fromErrorObject methodName err@(M.ObjectMap errorMap) =
    case lookup (M.ObjectStr "message") errorMap of
        Just (M.ObjectStr "invalid email or password") ->
            InvalidEmailOrPassword
        Just (M.ObjectStr "invalid talk_id") -> InvalidTalkId
        _ -> UnexpectedReponse methodName err
fromErrorObject methodName other = UnexpectedReponse methodName other

callRpc
    :: RPC.Client
    -> RPC.MethodName
    -> [M.Object]
    -> IO (Either Exception M.Object)
callRpc rpcClient methodName args = do
    eres <- RPC.call rpcClient methodName args
    return (fromErrorObject methodName `fmapL` eres)

callRpcThrow :: RPC.Client -> RPC.MethodName -> [M.Object] -> IO M.Object
callRpcThrow rpcClient methodName args = do
    eres <- callRpc rpcClient methodName args
    case eres of
        Left  e   -> E.throwIO e
        Right obj -> return obj

convertOrThrow :: RPC.MethodName -> (M.Object -> Maybe a) -> M.Object -> IO a
convertOrThrow methodName conv obj = case conv obj of
    Just x  -> return x
    Nothing -> E.throwIO $ UnexpectedReponse methodName obj
