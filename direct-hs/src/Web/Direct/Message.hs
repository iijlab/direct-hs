{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Direct.Message where

import           Control.Applicative ((<|>))
import           Control.Monad       (mapM)
import           Data.List           (elemIndex)
import           Data.Maybe          (maybeToList)
import qualified Data.MessagePack    as M
import qualified Data.Text           as T
import           Data.Word           (Word64)

import           Web.Direct.Types
import           Web.Direct.Utils

----------------------------------------------------------------

encodeMessage :: Message -> TalkId -> [M.Object]
encodeMessage (Txt text) tid =
    [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]
encodeMessage (Location addr url) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 1
    , M.ObjectStr (T.unlines ["今ココ：", addr, url])
    ]
encodeMessage (Stamp set idx Nothing) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 2
    , M.ObjectMap
        [ (M.ObjectStr "stamp_set"  , M.ObjectWord set)
        , (M.ObjectStr "stamp_index", M.ObjectWord idx)
        ]
    ]
encodeMessage (Stamp set idx (Just txt)) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 2
    , M.ObjectMap
        [ (M.ObjectStr "stamp_set"  , M.ObjectWord set)
        , (M.ObjectStr "stamp_index", M.ObjectWord idx)
        , (M.ObjectStr "text"       , M.ObjectStr txt)
        ]
    ]
encodeMessage (Files [file] Nothing) tid =
    [M.ObjectWord tid, M.ObjectWord 4, encodeFile file]
encodeMessage (Files files mtext) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 5
    , M.ObjectMap
        $ (M.ObjectStr "files", M.ObjectArray $ map encodeFile files)
        : maybeToList
              (fmap (\text -> (M.ObjectStr "text", M.ObjectStr text)) mtext)
    ]
encodeMessage (YesNoQ qst) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 500
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (YesNoA qst ans) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 501
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "response", M.ObjectBool ans)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectQ qst opt) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 502
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "options" , M.toObject opt)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectA qst opt ans) tid =
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
encodeMessage (TaskQ ttl cls) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 504
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        ]
    ]
encodeMessage (TaskA ttl cls don) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 505
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , (M.ObjectStr "closing_type", M.ObjectWord (if cls then 1 else 0))
        , (M.ObjectStr "done"        , M.ObjectBool don)
        ]
    ]

encodeMessage (Other text) tid =
    [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]

encodeFile :: File -> M.Object
encodeFile File {..} = M.ObjectMap
    [ (M.ObjectStr "url"         , M.ObjectStr fileUrl)
    , (M.ObjectStr "file_id"     , M.ObjectWord fileId)
    , (M.ObjectStr "name"        , M.ObjectStr fileName)
    , (M.ObjectStr "content_type", M.ObjectStr fileContentType)
    , (M.ObjectStr "content_size", M.ObjectWord fileContentSize)
    ]

----------------------------------------------------------------

decodeMessage
    :: [(M.Object, M.Object)] -> Maybe (Message, MessageId, TalkId, UserId)
decodeMessage rspinfo = do
    M.ObjectWord tid <- look "talk_id" rspinfo
    M.ObjectWord mid <- look "message_id" rspinfo
    M.ObjectWord uid <- look "user_id" rspinfo
    msg              <- getMessage
    return (msg, mid, tid, uid)
  where
    getMessage = do
        typ <- look "type" rspinfo
        case typ of
            M.ObjectWord 1 -> do
                text <- look "content" rspinfo >>= M.fromObject
                if "今ココ：" `T.isPrefixOf` text
                    then
                        let ln   = T.lines text
                            addr = ln !! 1
                            url  = ln !! 2
                        in  return (Location addr url)
                    else return (Txt text)
            M.ObjectWord 2 -> do
                M.ObjectMap m <- look "content" rspinfo
                set           <- look "stamp_set" m >>= M.fromObject
                idx           <- look "stamp_index" m >>= M.fromObject
                let txt = look "text" m >>= M.fromObject
                return (Stamp set idx txt)
            M.ObjectWord 4   -> decodeFilesMessage
            M.ObjectWord 5   -> decodeFilesMessage
            M.ObjectWord 500 -> do
                M.ObjectMap m <- look "content" rspinfo
                qst           <- look "question" m >>= M.fromObject
                return (YesNoQ qst)
            M.ObjectWord 501 -> do
                M.ObjectMap m <- look "content" rspinfo
                qst           <- look "question" m >>= M.fromObject
                ans           <- look "response" m >>= M.fromObject
                return (YesNoA qst ans)
            M.ObjectWord 502 -> do
                M.ObjectMap m <- look "content" rspinfo
                qst           <- look "question" m >>= M.fromObject
                opt           <- look "options" m >>= M.fromObject
                return (SelectQ qst opt)
            M.ObjectWord 503 -> do
                M.ObjectMap m <- look "content" rspinfo
                qst           <- look "question" m >>= M.fromObject
                opt           <- look "options" m >>= M.fromObject
                idx           <- look "response" m >>= M.fromObject
                let ans = opt !! fromIntegral (idx :: Word64)
                return (SelectA qst opt ans)
            M.ObjectWord 504 -> do
                M.ObjectMap m <- look "content" rspinfo
                ttl           <- look "title" m >>= M.fromObject
                cls'          <- look "closing_type" m >>= M.fromObject
                let cls = cls' == (1 :: Word64)
                return (TaskQ ttl cls)
            M.ObjectWord 505 -> do
                M.ObjectMap m <- look "content" rspinfo
                ttl           <- look "title" m >>= M.fromObject
                cls'          <- look "closing_type" m >>= M.fromObject
                don           <- look "done" m >>= M.fromObject
                let cls = cls' == (1 :: Word64)
                return (TaskA ttl cls don)
            _ -> return (Other $ T.pack $ show rspinfo)

    decodeFilesMessage = do
        obj@(M.ObjectMap m) <- look "content" rspinfo
        fs                  <- ((: []) <$> decodeFile obj) <|> decodeFiles m
        let text = case look "text" m of
                Just (M.ObjectStr t) -> Just t
                Just _other          -> Nothing
                Nothing              -> Nothing
        Just $ Files fs text

    decodeFiles m = do
        M.ObjectArray xs <- look "files" m
        mapM decodeFile xs

    decodeFile (M.ObjectMap f) = do
        M.ObjectWord fileId          <- look "file_id" f
        M.ObjectStr  fileName        <- look "name" f
        M.ObjectStr  fileContentType <- look "content_type" f
        M.ObjectWord fileContentSize <- look "content_size" f
        M.ObjectStr  fileUrl         <- look "url" f
        Just File {..}
    decodeFile _ = Nothing
