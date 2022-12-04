{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TupleSections #-}
-- cover all cases!
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
-- warn about incomplete patterns v2
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
-- write all your toplevel signatures!
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
-- use different names!
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
-- use all your pattern matches!
{-# OPTIONS_GHC -fwarn-unused-matches #-}

import           Control.Monad
import           Data.List        (foldl', sort)
import           Data.List.Unique
import           Data.Maybe       (fromJust)
import           Data.Strings     (strSplit, strSplitAll)
import           System.IO

-- 12.12.17
fullPairs :: [a] -> [(a, a)]
fullPairs = concat . go
  where
    go []       = []
    go (x : xs) = map (x,) xs : go xs

validatePassphrase :: (Eq a) => [a] -> Bool
validatePassphrase []       = True
validatePassphrase (x : xs) = notElem x xs && validatePassphrase xs

validatePassphrase' :: (Eq a, Ord a) => [([a], [a])] -> Bool
validatePassphrase' []            = True
validatePassphrase' ((x, y) : xs) = (sort x /= sort y) && validatePassphrase' xs

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let passphrases = map (strSplitAll " ") $ lines contents
  let numValid = length $ filter (== True) $ map validatePassphrase passphrases
  print numValid

  let wordPairs = map fullPairs passphrases
  let numAnagramValid = length $ filter (== True) $ map validatePassphrase' wordPairs
  print numAnagramValid

main :: IO ()
main =
  mainWork "input.txt"
