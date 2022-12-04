{-# LANGUAGE RankNTypes #-}
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
import           Data.List     (foldl')
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 08.12.21

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

parseLine :: String -> ([String], [String])
parseLine line = do
  let splitInputOutput = strSplitAll " | " line
  let input = head splitInputOutput
  let output = head $ tail splitInputOutput
  let resInput = strSplitAll " " input
  let resOutput = strSplitAll " " output
  (resInput, resOutput)

specialLengths :: [Int]
specialLengths = [2, 3, 4, 7]

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let lns = lines contents
  let parsed = map parseLine lns
  --print $ parsed
  let onlyOutput = map snd parsed
  let numSpecial = sum $ map (length . filter (\x -> length x `elem` specialLengths)) onlyOutput
  print numSpecial

main :: IO ()
main = do
  mainWork "input.txt"
