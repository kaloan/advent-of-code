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
import           Data.Foldable    (toList)
import           Data.List        (foldl', sort)
import           Data.List.Unique
import qualified Data.Map         as Map
import           Data.Sequence    hiding (lookup)
import           Data.Strings     (strSplit, strSplitAll)
import           System.IO

-- 22.12.20

play :: (Ord a) => Seq a -> Seq a -> Seq a
play Empty p2 = p2
play p1 Empty = p1
play (x :<| xs) (y :<| ys) =
  if x > y
    then play (xs |> x |> y) ys
    else play xs (ys |> y |> x)

calculateScore :: (Num a, Enum a) => Seq a -> a
calculateScore = go 1 0
  where
    go n acc l =
      case viewr l of
        EmptyR  -> acc
        xs :> x -> go (succ n) (acc + n * x) xs

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let (p1, p2) = mapPair (map readInt . tail . lines) (map readInt . tail . lines) $ strSplit "\n\n" contents
  let result = play (fromList p1) (fromList p2)
  let pointsAdded = calculateScore result
  print pointsAdded

main :: IO ()
main =
  mainWork "input.txt"
