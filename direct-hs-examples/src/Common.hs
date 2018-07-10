module Common (
    url
  , jsonFileName
  , dieWhenLeft
  , exitError
  , showMsg
  ) where

import           Data.List              (intercalate)
import qualified Data.MessagePack       as M
import qualified Data.MessagePack.RPC   as Msg
import qualified Data.Text              as T
import           System.Exit            (die)
import qualified Web.Direct             as D

----------------------------------------------------------------

url :: D.URL
url = "wss://api.direct4b.com/albero-app-server/api"

----------------------------------------------------------------

jsonFileName :: FilePath
jsonFileName = ".direct4b.json"

----------------------------------------------------------------

dieWhenLeft :: Either String a -> IO a
dieWhenLeft = either exitError return

exitError :: String -> IO a
exitError emsg = die $ "[ERROR] " ++ emsg ++ "\n"

----------------------------------------------------------------

showMsg :: Msg.Message -> String
showMsg (Msg.RequestMessage _ method objs) =
    "request " ++ T.unpack method ++ " " ++ showObjs objs
showMsg (Msg.ResponseMessage _ (Left  obj)) = "response error " ++ showObj obj
showMsg (Msg.ResponseMessage _ (Right obj)) = "response " ++ showObj obj
showMsg (Msg.NotificationMessage method objs) =
    "notification " ++ T.unpack method ++ " " ++ showObjs objs

showObjs :: [M.Object] -> String
showObjs objs = "[" ++ intercalate "," (map showObj objs) ++ "]"

showObj :: M.Object -> String
showObj (M.ObjectWord w)  = "+" ++ show w
showObj (M.ObjectInt  n)  = show n
showObj M.ObjectNil       = "nil"
showObj (M.ObjectBool  b) = show b
showObj (M.ObjectStr   s) = "\"" ++ T.unpack s ++ "\""
showObj (M.ObjectArray v) = "[" ++ intercalate "," (map showObj v) ++ "]"
showObj (M.ObjectMap   m) = "{" ++ intercalate "," (map showPair m) ++ "}"
    where showPair (x, y) = "(" ++ showObj x ++ "," ++ showObj y ++ ")"
showObj (M.ObjectBin _   ) = error "ObjectBin"
showObj (M.ObjectExt _ _ ) = error "ObjectExt"
showObj (M.ObjectFloat  _) = error "ObjectFloat"
showObj (M.ObjectDouble _) = error "ObjectDouble"

----------------------------------------------------------------

