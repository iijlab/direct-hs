module Web.Direct.Utils where

import qualified Data.MessagePack as M
import qualified Data.Text        as T

look :: T.Text -> [(M.Object, a)] -> Maybe a
look key = lookup (M.ObjectStr key)
