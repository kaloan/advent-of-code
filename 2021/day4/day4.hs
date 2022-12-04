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
import           Data.Array.MArray
import           Data.List         (foldl')
import           Data.Strings      (strSplit, strSplitAll)
import           System.IO

-- 04.12.21

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let (numsToParse, boardsToParse) = strSplit "\n\n" contents
  let nums = readIntList $ "[" ++ numsToParse ++ "]"
  let boards = strSplitAll "\n\n" boardsToParse
  --arrayExample :: MArray (STUArray s) Int (ST s)
  --let arrayExample = newArray ((0, 0), (4, 4)) 0
  let arrayExample = newArray ((0, 0), (4, 4)) '0'
  print $ show arrayExample

main :: IO ()
main = do
  mainWork "test.txt"
