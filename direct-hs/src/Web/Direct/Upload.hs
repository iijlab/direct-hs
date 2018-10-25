{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Web.Direct.Upload
    ( UploadFile(..)
    , UploadAuth(..)
    , readToUpload
    , runUploadFile
    ) where

import qualified Data.ByteString.Lazy      as BL
import qualified Data.Text                 as T
import qualified Data.Text.Encoding        as TE
import           Data.Word                 (Word64)
import qualified Network.HTTP.Client       as H
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Network.HTTP.Types.Status (Status (statusCode))
import qualified System.FilePath           as FP

import           Web.Direct.Exception
import           Web.Direct.Types


-- | Arguments for 'uploadFile'.
--   Currently, only one file can be uploaded per once.
data UploadFile = UploadFile
    { uploadFileAttachedText :: !(Maybe T.Text)
    , uploadFileMimeType     :: !T.Text
    , uploadFileName         :: !T.Text
    , uploadFileContent      :: !BL.ByteString
    , uploadFileSize         :: !Word64
    }


readToUpload :: Maybe T.Text -> T.Text -> FilePath -> IO UploadFile
readToUpload uploadFileAttachedText uploadFileMimeType path
    = do
        let uploadFileName = T.pack $ FP.takeFileName path
        uploadFileContent <- BL.readFile path
        let uploadFileSize = fromIntegral $ BL.length uploadFileContent
        return UploadFile {..}


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
