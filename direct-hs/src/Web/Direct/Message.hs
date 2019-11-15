{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Direct.Message where

import           Control.Applicative ((<|>))
import           Data.Maybe          (maybeToList)
import qualified Data.MessagePack    as M
import qualified Data.Text           as T

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
encodeMessage (YesNoQ (YesNoQuestion qst ct)) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 500
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , encodeClosingType ct
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (YesNoA (YesNoAnswer qst ct ans irl)) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 501
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , encodeClosingType ct
        , (M.ObjectStr "response", M.ObjectBool ans)
        , (M.ObjectStr "in_reply_to", M.ObjectWord irl)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectQ (SelectQuestion qst opt ct)) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 502
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "options" , M.toObject opt)
        , encodeClosingType ct
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (SelectA (SelectAnswer qst opt ct ans irl)) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 503
    , M.ObjectMap
        [ (M.ObjectStr "question", M.ObjectStr qst)
        , (M.ObjectStr "options" , M.toObject opt)
        , encodeClosingType ct
        , (M.ObjectStr "response", M.toObject ans)
        , (M.ObjectStr "in_reply_to", M.ObjectWord irl)
        , (M.ObjectStr "listing" , M.ObjectBool False)
        ]
    ]
encodeMessage (TaskQ (TaskQuestion ttl ct)) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 504
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , encodeClosingType ct
        ]
    ]
encodeMessage (TaskA (TaskAnswer ttl ct don irl)) tid =
    [ M.ObjectWord tid
    , M.ObjectWord 505
    , M.ObjectMap
        [ (M.ObjectStr "title"       , M.ObjectStr ttl)
        , encodeClosingType ct
        , (M.ObjectStr "done"        , M.ObjectBool don)
        , (M.ObjectStr "in_reply_to", M.ObjectWord irl)
        ]
    ]

encodeMessage (Other text) tid =
    [M.ObjectWord tid, M.ObjectWord 1, M.ObjectStr text]

encodeClosingType :: ClosingType -> (M.Object, M.Object)
encodeClosingType ct = (M.ObjectStr "closing_type", M.ObjectWord w)
  where
    w = case ct of
            OnlyOne -> 0
            Anyone  -> 1

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
                YesNoQ <$> decodeYesNoQuestion m
            M.ObjectWord 501 -> do
                M.ObjectMap m <- look "content" rspinfo
                YesNoQuestion qst ct <- decodeYesNoQuestion m
                ans           <- look "response" m >>= M.fromObject
                irl           <- decodeInReplyTo m
                return . YesNoA $ YesNoAnswer qst ct ans irl
            M.ObjectWord 502 -> do
                M.ObjectMap m <- look "content" rspinfo
                SelectQ <$> decodeSelectQuestion m
            M.ObjectWord 503 -> do
                M.ObjectMap m <- look "content" rspinfo
                SelectQuestion qst opt ct <- decodeSelectQuestion m
                ans           <- look "response" m >>= M.fromObject
                irl           <- decodeInReplyTo m
                return . SelectA $ SelectAnswer qst opt ct ans irl
            M.ObjectWord 504 -> do
                M.ObjectMap m <- look "content" rspinfo
                TaskQ <$> decodeTaskQuestion m
            M.ObjectWord 505 -> do
                M.ObjectMap m <- look "content" rspinfo
                TaskQuestion ttl ct <- decodeTaskQuestion m
                don           <- look "done" m >>= M.fromObject
                irl           <- decodeInReplyTo m
                return . TaskA $ TaskAnswer ttl ct don irl
            _ -> return (Other $ T.pack $ show rspinfo)

    decodeYesNoQuestion m = do
        qst <- look "question" m >>= M.fromObject
        ct  <- decodeClosingType m
        return $ YesNoQuestion qst ct

    decodeSelectQuestion m = do
        qst <- look "question" m >>= M.fromObject
        opt <- look "options" m >>= M.fromObject
        ct  <- decodeClosingType m
        return $ SelectQuestion qst opt ct

    decodeTaskQuestion m = do
        ttl <- look "title" m >>= M.fromObject
        ct  <- decodeClosingType m
        return $ TaskQuestion ttl ct

    decodeClosingType m =
        case look "closing_type" m of
            Just o -> case o of
                    M.ObjectWord 1 -> Just Anyone
                    M.ObjectWord 0 -> Just OnlyOne
                    _              -> Nothing
            Nothing -> Just OnlyOne

    decodeInReplyTo m = do
        M.ObjectWord mid <- look "in_reply_to" m
        return mid

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

----------------------------------------------------------------

-- | Get the answer as a text from 'Web.Direct.SelectA'
getSelectedAnswer :: SelectAnswer -> T.Text
getSelectedAnswer (SelectAnswer _qst opt _ct res _irl) = opt !! res
