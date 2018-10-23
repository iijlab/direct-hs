{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Direct.Upload
    ( UploadFile(..)
    , UploadAuth(..)
    , readToUpload
    , decodeUploadAuth
    , toCreateUploadAuth
    , runUploadFile
    ) where

import           Control.Error             (note)
import qualified Data.ByteString.Lazy      as BL
import qualified Data.MessagePack          as M
import           Data.MessagePack.RPC      (MethodName)
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Word                 (Word64)
import qualified Network.HTTP.Client       as H
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (Status (statusCode))
import qualified System.FilePath           as FP

import           Web.Direct.Exception
import           Web.Direct.Types


-- | Currently, only one file can be uploaded per once.
data UploadFile = UploadFile
    { uploadFileDomainId     :: !DomainId
    , uploadFileAttachedText :: !(Maybe T.Text)
    , uploadFileMimeType     :: !T.Text
    , uploadFileName         :: !T.Text
    , uploadFileContent      :: !BL.ByteString
    , uploadFileSize         :: !Word64
    }


data UploadAuth = UploadAuth
    { uploadAuthGetUrl             :: !T.Text
    , uploadAuthPutUrl             :: !T.Text
    , uploadAuthFileId             :: !FileId
    , uploadAuthContentDisposition :: !T.Text
    }


readToUpload :: DomainId -> Maybe T.Text -> T.Text -> FilePath -> IO UploadFile
readToUpload uploadFileDomainId uploadFileAttachedText uploadFileMimeType path
    = do
        let uploadFileName = T.pack $ FP.takeFileName path
        uploadFileContent <- BL.readFile path
        let uploadFileSize = fromIntegral $ BL.length uploadFileContent
        return UploadFile {..}


toCreateUploadAuth :: UploadFile -> [M.Object]
toCreateUploadAuth UploadFile {..} =
    [ M.ObjectStr uploadFileName
    , M.ObjectStr uploadFileMimeType
    , M.ObjectWord uploadFileSize
    , M.ObjectWord uploadFileDomainId
    ]


decodeUploadAuth
    :: MethodName
    -> M.Object
    -> [(M.Object, M.Object)]
    -> Either Exception UploadAuth
decodeUploadAuth methodName rsp rspMap =
    note (UnexpectedReponse methodName rsp) $ do
        M.ObjectStr uploadAuthGetUrl <- lookup (M.ObjectStr "get_url") rspMap
        M.ObjectStr uploadAuthPutUrl <- lookup (M.ObjectStr "put_url") rspMap
        M.ObjectWord uploadAuthFileId <- lookup (M.ObjectStr "file_id") rspMap

        M.ObjectMap formObj <- lookup (M.ObjectStr "post_form") rspMap
        M.ObjectStr uploadAuthContentDisposition <- lookup
            (M.ObjectStr "Content-Disposition")
            formObj
        return UploadAuth {..}


runUploadFile :: UploadFile -> UploadAuth -> IO (Either Exception ())
runUploadFile UploadFile {..} UploadAuth {..} = do
    mgr <- H.newManager tlsManagerSettings
    req0    <- H.parseRequest $ T.unpack uploadAuthPutUrl
    let
        req = req0
            { H.method         = "PUT"
            , H.requestHeaders = [ ( "Content-Disposition"
                                   , TE.encodeUtf8 uploadAuthContentDisposition
                                   )
                                 , ( "Content-Type"
                                   , TE.encodeUtf8 uploadFileMimeType
                                   )
                                 ]
            , H.requestBody    = H.RequestBodyLBS uploadFileContent
            }
    res <- H.httpNoBody req mgr
    let st = H.responseStatus res
        sc = statusCode st
    if 200 <= sc && sc < 300
      then return $ Right ()
      else return $ Left $ UnexpectedReponseWhenUpload st
