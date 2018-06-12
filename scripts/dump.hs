-- % sudo tcpdump -i en0 -s 0 -x -w direct.pcap port 443
-- % SSLKEYLOGFILE=~/tls_key.log "/Applications/Firefox.app/Contents/MacOS/firefox-bin"
-- % runghc dump.hs direct.pcap tls_key.log

module Main (main) where

import qualified Data.Text as T
import Control.Monad
import qualified Data.ByteString.Lazy.Char8 as CL
import Data.Hex
import Data.IORef
import Data.List
import Data.List.Split
import qualified Data.MessagePack as M
import System.Environment
import System.IO
import System.Process

main :: IO ()
main = do
    [pcap,keylog] <- getArgs
    let process = proc "tshark" ["-V","-o","ssl.keylog_file:"++keylog,"-r",pcap]
    (_,Just out,_,_) <- createProcess process{std_out = CreatePipe}
    hSetBuffering out LineBuffering
    inp <- lines <$> hGetContents out
    ref <- newIORef ""
    visualize ref inp

visualize :: IORef String -> [String] -> IO ()
visualize _       [] = return ()
visualize ref (l:ls) = do
    when ("Internet Protocol Version" `isPrefixOf` l) $ do
        writeIORef ref $ drop 29 l
    if "Data" `isPrefixOf` l then do
        readIORef ref >>= putStrLn
        (ls', block) <- extract (tail ls) []
        let Just obj = M.unpack $ CL.pack $ cook block
        putStr $ showObj obj
        putStr "\n\n"
        visualize ref ls'
      else
        visualize ref ls
  where
    extract []  block = return ([], block)
    extract xxs@(x:xs) block
      | head x /= ' ' = do
          extract xs (x:block)
      | otherwise = do
          return (xxs, reverse block)
    cook xs = h
      where
        oneline = concat $ map concat $ map (splitOn " ") $ map (take 47 . drop 6) xs
        Just h = unhex oneline

showObj :: M.Object -> String
showObj (M.ObjectWord  w) = "+" ++ show w
showObj (M.ObjectInt   n) = show n
showObj  M.ObjectNil      = "nil"
showObj (M.ObjectBool  b) = show b
showObj (M.ObjectStr   s) = "\"" ++ T.unpack s ++ "\""
showObj (M.ObjectArray v) = "[" ++ intercalate "," (map showObj v) ++ "]"
showObj (M.ObjectMap   m) = "{" ++ intercalate "," (map showPair m) ++ "}"
  where
    showPair (x,y) = "(" ++ showObj x ++ "," ++ showObj y ++ ")"
showObj (M.ObjectBin _)    = error "ObjectBin"
showObj (M.ObjectExt _ _)  = error "ObjectExt"
showObj (M.ObjectFloat _)  = error "ObjectFloat"
showObj (M.ObjectDouble _) = error "ObjectDouble"
