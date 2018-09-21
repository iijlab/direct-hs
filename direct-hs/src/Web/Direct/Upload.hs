{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Direct.Upload
    ( UploadFile(..)
    , readToUpload
    , decodeUploadAuth
    , toCreateUploadAuth
    , runUploadFile
    ) where

import           Control.Error        (note)
import qualified Data.ByteString.Lazy as BL
import qualified Data.MessagePack     as M
import           Data.MessagePack.RPC (MethodName)
import qualified Data.Text            as T
import           Data.Word            (Word64)

import           Web.Direct.Exception
import           Web.Direct.Types


data UploadFile = UploadFile
    { uploadFileContent  :: !BL.ByteString
    , uploadFileName     :: !T.Text
    , uploadFileMimeType :: !(Maybe T.Text)
    , uploadFileSize     :: !Word64
    }


-- TODO: Send with PUT.
-- TODO: As long as I remember,
-- all values in post_form of the response of create_upload_auth are appended as the query parameter
data UploadAuth = UploadAuth
    { uploadAuthGetUrl             :: !T.Text
    , uploadAuthPutUrl             :: !T.Text
    , uploadAuthFileId             :: !FileId
    , uploadAuthContentDisposition :: !T.Text
    }


readToUpload :: Maybe T.Text -> FilePath -> IO UploadFile
readToUpload mimeType path =
    error "TODO: readToUpload is not defined yet!"


toCreateUploadAuth :: UploadFile -> [M.Object]
toCreateUploadAuth _ =
    error "TODO: toCreateUploadAuth is not defined yet!"


decodeUploadAuth :: MethodName -> M.Object -> [(M.Object, M.Object)] -> Either Exception UploadAuth
decodeUploadAuth methodName rsp rspMap =
    note (UnexpectedReponse methodName rsp) $ do
        M.ObjectStr uploadAuthGetUrl             <- lookup (M.ObjectStr "get_url") rspMap
        M.ObjectStr uploadAuthPutUrl             <- lookup (M.ObjectStr "put_url") rspMap
        M.ObjectWord uploadAuthFileId            <- lookup (M.ObjectStr "file_id") rspMap

        M.ObjectMap formObj                      <- lookup (M.ObjectStr "post_form") rspMap
        M.ObjectStr uploadAuthContentDisposition <- lookup (M.ObjectStr "Content-Disposition") formObj
        return UploadAuth {..}


runUploadFile :: UploadFile -> UploadAuth -> IO (Either Exception ())
runUploadFile UploadFile {..} UploadAuth {..} =
    error "TODO: runUploadFile is not defined yet!"
    -- TODO: run httpNoBody to put_url with PUT, add header Content-Type and Content-Disposition from post_form
