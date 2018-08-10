{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Exception
    ( Exception(..)
    , callRpcThrow
    , resultToObjectOrException
    ) where


import           Control.Error                            (fmapL)
import qualified Control.Exception                        as E
import qualified Data.MessagePack                         as M
import qualified Data.MessagePack.RPC                     as RPC
import           Data.Typeable                            (Typeable)
import qualified Network.MessagePack.RPC.Client.WebSocket as RPC


data Exception =
      InvalidEmailOrPassword
    | InvalidTalkId
    | InvalidWsUrl !String
    | UnexpectedReponse !RPC.MethodName !M.Object
  deriving (Eq, Show, Typeable)

instance E.Exception Exception


resultToObjectOrException
    :: RPC.MethodName -> RPC.Result -> Either Exception M.Object
resultToObjectOrException methodName = fmapL $ \case
    err@(M.ObjectMap errorMap) ->
        case lookup (M.ObjectStr "message") errorMap of
            Just (M.ObjectStr "invalid email or password") ->
                InvalidEmailOrPassword
            Just (M.ObjectStr "invalid talk_id") -> InvalidTalkId
            _ -> UnexpectedReponse methodName err
    other -> UnexpectedReponse methodName other


callRpcThrow :: RPC.Client -> RPC.MethodName -> [M.Object] -> IO M.Object
callRpcThrow rpcClient methodName args =
    either E.throwIO return
        =<< resultToObjectOrException methodName
        <$> RPC.call rpcClient methodName args
