#!/usr/bin/env stack
-- stack --resolver lts-11.4 script --package text
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad (mapM_)
import           Data.Bits ((.|.), (.&.), shiftL)
import           Data.Char (isDigit)
import qualified Data.Text as T
import           Data.Word (Word64)
import           System.Environment (getArgs)
import           Text.Read (readMaybe)


-- |
-- Parse an unsinged 64 bit integer represented asthe format
-- internally used by the direct's web-browser-based client.
-- The format is like below:
--
-- @
--     <The upper 32 bits: singed integer>_<The lower 32 bits: signed integer>
-- @
--
-- That is, it's signed 32 bit integers separated by an underscore.
-- Note some non-digit suffix characters can be placed.
--
-- Here are the examples:
--   talk-_176215528_2025848832
--   _176070168_-1744830464
main :: IO ()
main = do
  args <- getArgs
  mapM_ putStrLn $ zipWith format args $ map parse args


parse :: String -> Maybe Integer
parse s =
  case T.splitOn "_" $ T.dropAround (not . isDigit) $ T.pack s of
      [h, l] ->
        shiftAndAdd <$> readMaybe (T.unpack h) <*> readMaybe (T.unpack l)
      _ -> Nothing
  where
    shiftAndAdd h l = (h `shiftL` 32) .|. getLower32bits l
    getLower32bits = (.&. 0xffffffff)


format :: String -> Maybe Integer -> String
format input result =
  input ++ ": " ++ maybe "Invalid" show result
