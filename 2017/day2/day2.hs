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

evenlyDivisible :: (Integral a) => [(a, a)] -> Maybe a
evenlyDivisible [] = Nothing
evenlyDivisible ((x, y) : xs)
  | x `mod` y == 0 = Just $ x `div` y
  | y `mod` x == 0 = Just $ y `div` x
  | otherwise = evenlyDivisible xs

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let lns = lines contents
  let delim = if filename == "input.txt" then "\t" else " "
  let rows = map (map readInt . strSplitAll delim) lns
  let checksum = sum $ map (\row -> maximum row - minimum row) rows
  print checksum
  let allPairsRows = map fullPairs rows
  let findEvenlyDivisibleSums = map (fromJust . evenlyDivisible) allPairsRows
  let divisibleChecksum = sum findEvenlyDivisibleSums
  print divisibleChecksum

main :: IO ()
main =
  mainWork "input.txt"
