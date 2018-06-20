-- | Implementation of [MessagePack RPC](https://github.com/msgpack-rpc/msgpack-rpc/blob/master/spec.md)

module Data.MessagePack.RPC (
    MessageId
  , MethodName
  , Message(..)
  ) where

import           Data.MessagePack (MessagePack(..), Object(..))
import qualified Data.Text as T
import           Data.Word (Word64)

-- | Message ID.
type MessageId = Word64

-- | Method name.
type MethodName = T.Text

-- | Message type of MessagePack PRC.
--   Use 'toObject' and 'fromObject' for conversion.
data Message =
    -- | Request
      RequestMessage MessageId MethodName [Object]
    -- | Response. 'Left' is an error. 'Right' is a result.
    | ResponseMessage MessageId (Either Object Object)
    -- | Notification.
    | NotificationMessage MethodName [Object]
  deriving (Eq, Show)

instance MessagePack Message where
  toObject (RequestMessage mid methodName args) =
    ObjectArray
      [ ObjectWord 0
      , ObjectWord mid
      , ObjectStr methodName
      , ObjectArray args
      ]

  toObject (ResponseMessage mid (Right result)) =
    ObjectArray
      [ ObjectWord 1
      , ObjectWord mid
      , ObjectNil
      , result
      ]

  toObject (ResponseMessage mid (Left err)) =
    ObjectArray
      [ ObjectWord 1
      , ObjectWord mid
      , err
      , ObjectNil
      ]

  toObject (NotificationMessage methodName params) =
    ObjectArray
      [ ObjectWord 2
      , ObjectStr methodName
      , ObjectArray params
      ]

  fromObject
    ( ObjectArray
        [ ObjectWord 0
        , ObjectWord mid
        , ObjectStr methodName
        , ObjectArray args
        ]
    ) =
      return $ RequestMessage mid methodName args

  fromObject
    ( ObjectArray
        [ ObjectWord 1
        , ObjectWord mid
        , ObjectNil
        , result
        ]
    ) =
      return $ ResponseMessage mid (Right result)
  fromObject
    ( ObjectArray
        [ ObjectWord 1
        , ObjectWord mid
        , err
        , ObjectNil
        ]
    ) =
      return $ ResponseMessage mid (Left err)

  fromObject
    ( ObjectArray
        [ ObjectWord 2
        , ObjectStr methodName
        , ObjectArray params
        ]
    ) =
      return $ NotificationMessage methodName params

  fromObject other =
    fail $ "Unexpected object:" ++ show other
