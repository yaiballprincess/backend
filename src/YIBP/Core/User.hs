module YIBP.Core.User (checkPassword, checkUsername) where

import Data.Char
import Data.Text qualified as T

count :: (Char -> Bool) -> T.Text -> Int
count predicate = T.foldl' go 0
  where
    go :: Int -> Char -> Int
    go acc ch = if predicate ch then acc + 1 else acc

checkPassword :: T.Text -> Bool
checkPassword password =
  (T.length password >= 8)
    && (count isLetter password >= 4)
    && (count isDigit password >= 2)

checkUsername :: T.Text -> Bool
checkUsername = T.any (\ch -> isLetter ch || isDigit ch)