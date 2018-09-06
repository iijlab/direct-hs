{-# LANGUAGE OverloadedStrings #-}

module Web.Direct.Message where

import           Data.List        (elemIndex)
import qualified Data.MessagePack as M
import qualified Data.Text        as T
import           Data.Word        (Word64)

import           Web.Direct.Types
import           Web.Direct.Utils

----------------------------------------------------------------

-- | Type for Direct messages.
data Message =
    Txt       !T.Text
  | Location  !T.Text !T.Text -- Address, GoogleMap URL
  | Stamp     !Word64 !Word64 !(Maybe T.Text)
  | YesNoQ    !T.Text
  | YesNoA    !T.Text Bool
  | SelectQ   !T.Text ![T.Text]
  | SelectA   !T.Text ![T.Text] T.Text
  | TaskQ     !T.Text Bool -- False: anyone, True: everyone
  | TaskA     !T.Text Bool Bool -- done
  | Other     !T.Text
  deriving (Eq, Show)

----------------------------------------------------------------

-- | Auxiliary data to identify communication.
data Aux = Aux {
    auxTalkId    :: !TalkId
  , auxMessageId :: !MessageId
  , auxUserId    :: !UserId
  } deriving (Eq, Show)

-- | The default 'Aux'.
--   This can be used in the main 'IO' where 'Aux' is not available.
--   To use 'sendMessage' in the main 'IO', set talk room ID to
--   'defaultAux'.
defaultAux :: Aux
defaultAux = Aux 0 0 0

----------------------------------------------------------------

encodeMessage :: Message -> Aux -> [M.Object]
encodeMessage (Txt text) (Aux tid _ _) =
    [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]
encodeMessage (Location addr url) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 1
    , M.ObjectStr (T.unlines ["今ココ：", addr, url])
    ]
encodeMessage (Stamp set idx Nothing) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 2
    , M.ObjectMap
        [ (M.ObjectStr "stamp_set"  , M.ObjectWord set)
        , (M.ObjectStr "stamp_index", M.ObjectWord idx)
        ]
    ]
encodeMessage (Stamp set idx (Just txt)) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 2
    , M.ObjectMap
        [ (M.ObjectStr "stamp_set"  , M.ObjectWord set)
        , (M.ObjectStr "stamp_index", M.ObjectWord idx)
        , (M.ObjectStr "text"       , M.ObjectStr txt)
        ]
    ]
encodeMessage (YesNoQ qst) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 500
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (YesNoA qst ans) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 501
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "response", M.ObjectBool ans)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectQ qst opt) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 502
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "options" , M.toObject opt)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectA qst opt ans) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 503
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "options" , M.toObject opt)
        , (M.ObjectStr "response", M.ObjectWord (fromIntegral idx))
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
    where Just idx = ans `elemIndex` opt -- fixme
encodeMessage (TaskQ ttl cls) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 504
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        ]
    ]
encodeMessage (TaskA ttl cls don) (Aux tid _ _) =
    [ M.ObjectWord tid
    , M.ObjectWord 505
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        , (M.ObjectStr "done"        , M.ObjectBool don)
        ]
    ]

encodeMessage (Other text) (Aux tid _ _) =
    [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]

----------------------------------------------------------------

decodeMessage :: [(M.Object, M.Object)] -> Maybe (Message, Aux)
decodeMessage rspinfo = do
    M.ObjectWord tid <- look "talk_id" rspinfo
    M.ObjectWord mid <- look "message_id" rspinfo
    M.ObjectWord uid <- look "user_id" rspinfo
    let aux = Aux tid mid uid
    typ <- look "type" rspinfo
    case typ of
        M.ObjectWord 1 -> do
            text <- look "content" rspinfo >>= M.fromObject
            if "今ココ：" `T.isPrefixOf` text
                then
                    let ln   = T.lines text
                        addr = ln !! 1
                        url  = ln !! 2
                    in  Just (Location addr url, aux)
                else Just (Txt text, aux)
        M.ObjectWord 2 -> do
            M.ObjectMap m <- look "content" rspinfo
            set           <- look "stamp_set" m >>= M.fromObject
            idx           <- look "stamp_index" m >>= M.fromObject
            let txt = look "text" m >>= M.fromObject
            Just (Stamp set idx txt, aux)
        M.ObjectWord 500 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            Just (YesNoQ qst, aux)
        M.ObjectWord 501 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            ans           <- look "response" m >>= M.fromObject
            Just (YesNoA qst ans, aux)
        M.ObjectWord 502 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            opt           <- look "options" m >>= M.fromObject
            Just (SelectQ qst opt, aux)
        M.ObjectWord 503 -> do
            M.ObjectMap m <- look "content" rspinfo
            qst           <- look "question" m >>= M.fromObject
            opt           <- look "options" m >>= M.fromObject
            idx           <- look "response" m >>= M.fromObject
            let ans = opt !! fromIntegral (idx :: Word64)
            Just (SelectA qst opt ans, aux)
        M.ObjectWord 504 -> do
            M.ObjectMap m <- look "content" rspinfo
            ttl           <- look "title" m >>= M.fromObject
            cls'          <- look "closing_type" m >>= M.fromObject
            let cls = cls' == (1 :: Word64)
            Just (TaskQ ttl cls, aux)
        M.ObjectWord 505 -> do
            M.ObjectMap m <- look "content" rspinfo
            ttl           <- look "title" m >>= M.fromObject
            cls'          <- look "closing_type" m >>= M.fromObject
            don           <- look "done" m >>= M.fromObject
            let cls = cls' == (1 :: Word64)
            Just (TaskA ttl cls don, aux)
        _ -> Just (Other $ T.pack $ show rspinfo, aux)
