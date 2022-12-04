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
import           Data.Array
import           Data.List     (foldl', sort)
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 07.12.21

absDiffOfConst :: (Num a) => [a] -> a -> a
absDiffOfConst l c = sum $ map abs $ zipWith (-) l (replicate (length l) c)

modifiedDiffOfConst :: (Num a, Integral a) => [a] -> a -> Double
modifiedDiffOfConst l c =
  sum
    $ map ((\x -> fromIntegral (x * (x + 1)) / 2.0). abs)
    $ zipWith (-) l (replicate (length l) c)


average :: (Num a, Integral a) => [a] -> Double
average l = fromIntegral (sum l) / fromIntegral (length l)

medianInList :: (Num a, Ord a, Integral a) => [a] -> a
medianInList l
  | odd len = l !! div (len + 1) 2
  | otherwise = go (l !! div len 2) (l !! (1 + div len 2))
  where
    len = length l
    go x y =
      if absDiffOfConst l x < absDiffOfConst l y
        then x
        else y
      -- | (x + y) `div` 2 == floor (fromIntegral (x + y) / 2.0) = (x + y) `div` 2
      -- | otherwise =
      --   if absDiffOfConst l x < absDiffOfConst l y
      --     then x
      --     else y

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let crabPositions = sort $ parseIntList contents
  let len = length crabPositions
  -- The median optimises E(|X-c|)
  let median = medianInList crabPositions
  print median
  print $ absDiffOfConst crabPositions median

mainWork' :: FilePath -> IO ()
mainWork' filename = do
  contents <- readFile filename
  let crabPositions = sort $ parseIntList contents
  let avg = average crabPositions
  -- let minModifiedDiff = foldl' (\acc x -> if modifiedDiffOfConst crabPositions x < modifiedDiffOfConst crabPositions acc then x else acc) 10000000000 [minimum crabPositions .. maximum crabPositions]
  -- print $ modifiedDiffOfConst crabPositions minModifiedDiff
  -- The average value should either be the solution or a great heuristic for any input with range more than 3-4.
  --As it optimises E((X-c)^2) it will most likely optimise E(0.5 * ((X-c)^2 + (X-c)))
  print $ modifiedDiffOfConst crabPositions (floor avg)
  print $ modifiedDiffOfConst crabPositions (ceiling avg)

main :: IO ()
main =
  --mainWork "input.txt"
  mainWork' "input.txt"
