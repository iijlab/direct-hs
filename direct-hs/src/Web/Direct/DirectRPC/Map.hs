{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Direct.DirectRPC.Map where

import qualified Data.List        as L
import           Data.Maybe       (mapMaybe)
import qualified Data.MessagePack as M


import           Web.Direct.Types
import           Web.Direct.Utils

----------------------------------------------------------------

fromCreateSession :: M.Object -> Maybe User
fromCreateSession (M.ObjectMap m) = do
    user <- look "user" m
    decodeUser user
fromCreateSession _ = Nothing

fromGetAcquaintances :: M.Object -> [(DomainId, [User])]
fromGetAcquaintances (M.ObjectArray xs) = mapMaybe fromGetAcquaintances' xs
  where
    fromGetAcquaintances' (M.ObjectArray [M.ObjectWord _domain, M.ObjectArray users])
        = Just (_domain, mapMaybe decodeUser users)
    fromGetAcquaintances' _ = Nothing
fromGetAcquaintances _ = []

decodeUser :: M.Object -> Maybe User
decodeUser (M.ObjectMap user) = do
    M.ObjectWord uid     <- look "user_id" user
    M.ObjectStr  dname   <- look "display_name" user
    M.ObjectStr  cdname  <- look "canonical_display_name" user
    M.ObjectStr  pdname  <- look "phonetic_display_name" user
    M.ObjectStr  cpdname <- look "canonical_phonetic_display_name" user
    Just $ User uid dname cdname pdname cpdname
decodeUser _ = Nothing

fromGetDomains :: M.Object -> [Domain]
fromGetDomains (M.ObjectArray arr) = mapMaybe decodeDomain arr
fromGetDomains _                   = []

decodeDomain :: M.Object -> Maybe Domain
decodeDomain (M.ObjectMap m) = do
    M.ObjectWord did   <- look "domain_id" m
    M.ObjectMap  s     <- look "domain" m
    M.ObjectStr  dname <- look "domain_name" s
    Just $ Domain did dname
decodeDomain _ = Nothing

fromGetTalks :: [User] -> M.Object -> [TalkRoom]
fromGetTalks users (M.ObjectArray arr) = mapMaybe (decodeTalkRoom users) arr
fromGetTalks _     _                   = []

decodeTalkRoom :: [User] -> M.Object -> Maybe TalkRoom
decodeTalkRoom (me : others) (M.ObjectMap m) = do
    M.ObjectWord tid <- look "talk_id" m
    M.ObjectWord tp  <- look "type" m
    let typ
            | tp == 1 = PairTalk
            | tp == 2 = case look "talk_name" m of
                Just (M.ObjectStr tname) -> GroupTalk tname
                _                        -> GroupTalk ""
            | otherwise = UnknownTalk
    M.ObjectArray uids <- look "user_ids" m
    let userIds = mapMaybe extract uids
        roomUsers =
            me
                : mapMaybe (\uid -> L.find (\u -> uid == userId u) others)
                           userIds
    Just $ TalkRoom tid typ roomUsers
  where
    extract (M.ObjectWord uid) = Just uid
    extract _                  = Nothing
decodeTalkRoom _ _ = Nothing

decodeUploadAuth :: [(M.Object, M.Object)] -> Maybe UploadAuth
decodeUploadAuth rspMap = do
    M.ObjectStr uploadAuthGetUrl <- lookup (M.ObjectStr "get_url") rspMap
    M.ObjectStr uploadAuthPutUrl <- lookup (M.ObjectStr "put_url") rspMap
    M.ObjectWord uploadAuthFileId <- lookup (M.ObjectStr "file_id") rspMap

    M.ObjectMap formObj <- lookup (M.ObjectStr "post_form") rspMap
    M.ObjectStr uploadAuthContentDisposition <- lookup
        (M.ObjectStr "Content-Disposition")
        formObj
    return UploadAuth {..}
