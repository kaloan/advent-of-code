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
import           Data.Bits
import           Data.List        (foldl', sort)
import           Data.List.Unique
import           Data.Strings     (strSplit, strSplitAll)
import           System.IO

-- 15.12.17

agree :: Int -> Int -> Int -> Bool
agree agreeNum x y = x .&. agreeNum == y .&. agreeNum

judge :: [(Int, Int)] -> Int -> Int -> Integer -> Integer
judge = go 0
  where
    go agreed iterList modder agreeNum 0 = agreed
    go agreed iterList modder agreeNum maxTimes =
      if all (agree agreeNum (fst $ head iterated) . fst) (tail iterated)
        then go (succ agreed) iterList modder agreeNum (pred maxTimes)
        else go agreed iterList modder agreeNum (pred maxTimes)
      where
        iterator (curr, toMult) = ((curr * toMult) .&. modder, toMult)
        iterated = map iterator iterList

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> Integer -> IO ()
mainWork filename iterCount = do
  contents <- readFile filename
  let (metaInfo, iterInfo) = strSplit "\n\n" contents
  let (modder, numBitAgree) = mapPair readInt readInt $ strSplit " " metaInfo
  let agreeNum = 2 ^ numBitAgree - 1
  let iters = map (mapPair readInt readInt . strSplit " ") $ lines iterInfo
  let findMatches = judge iters modder agreeNum iterCount
  -- print modder
  -- print agreeNum
  -- print iters
  print findMatches

main :: IO ()
main =
  mainWork "test.txt" 5
