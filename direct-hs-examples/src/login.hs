module Main (main) where

import qualified Control.Exception      as E
import qualified Data.ByteString.Lazy   as B
import qualified Data.Text              as T
import qualified Data.Text.IO           as T
import qualified System.Directory       as Dir
import           System.FilePath        ((</>))
import           System.IO              (BufferMode (NoBuffering), hGetEcho,
                                         hSetBuffering, hSetEcho, stderr, stdin,
                                         stdout)
import qualified Web.Direct             as D

import           Common

main :: IO ()
main = do
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    putStrLn $ "Parsed URL:" ++ show (D.directEndpointUrl D.defaultConfig)

    email <- getEmailAddress
    pwd <- getPassword
    eclient <- D.login D.defaultConfig email pwd
    case eclient of
        Left _       -> putStrLn "Logged failed,"
        Right client -> do
            putStrLn "Successfully logged in."

            B.writeFile jsonFileName
                $ D.serializePersistedInfo
                $ D.clientPersistedInfo client
            cd <- Dir.getCurrentDirectory
            putStrLn $ "Saved access token at '" ++ (cd </> jsonFileName) ++ "'."

getEmailAddress :: IO T.Text
getEmailAddress = do
    putStr "Enter direct email: "
    T.getLine

getPassword :: IO T.Text
getPassword = do
    putStr "Enter direct password: "
    t <- E.bracket (hGetEcho stdin)
                   (hSetEcho stdin)
                   (const (hSetEcho stdin False >> T.getLine))
    putStrLn ""
    return t
