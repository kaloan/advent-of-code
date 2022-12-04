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
import           Data.Char     (digitToInt)
import           Data.List     (foldl', sort)
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 12.12.17
readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let line = head $ lines contents
  let intList' = map digitToInt line
  let intList = intList' ++ [head intList']
  let pairs = zip intList (tail intList)
  let rollingSum = foldl' (\acc (l, r) -> if l == r then acc + l else acc) 0 pairs
  let halfwayPairs = zip intList' (drop (length intList `div` 2) intList')
  let halfwaySum = foldl' (\acc (l, r) -> if l == r then acc + l + r else acc) 0 halfwayPairs
  print rollingSum
  print halfwaySum

main :: IO ()
main =
  mainWork "input.txt"
