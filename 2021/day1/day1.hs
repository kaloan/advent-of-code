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
import           System.IO

-- 01.12.21

numberOfIncreases :: (Num a, Ord a) => [a] -> Int
numberOfIncreases l = length . filter (> 0) $ zipWith (-) (tail l) l

numberOfIncreases' :: (Num a, Ord a) => [a] -> Int
numberOfIncreases' l = length . filter (> 0) $ zipWith (-) (tail ll) ll
  where
    ll = zipWith (+) l $ zipWith (+) (tail l) (tail $ tail l)

readInt :: String -> Int
readInt = read

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let depthList = map readInt . lines $ contents
  --depthList <- map readInt . words $ contents
  print $ numberOfIncreases depthList
  print $ numberOfIncreases' depthList

main :: IO ()
main = do
  mainWork "input.txt"
