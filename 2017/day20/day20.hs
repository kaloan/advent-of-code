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
import           Data.Strings     (strReplace, strSplit, strSplitAll)
import           System.IO

-- 20.12.17
type Coord3D = (Int, Int, Int)

manhattanDistance :: (Num a) => [a] -> a
manhattanDistance = sum . map abs

minimizeWithFunc :: (Ord b, Ord a) => (a -> b) -> [a] -> a
-- minimizeWithFunc f = foldl' go 0
--   where
--     go acc x = if f x < f acc then x else acc
minimizeWithFunc f = snd . minimum . map (\x -> (f x, x))

parseGeneral :: String -> [Int]
parseGeneral = readIntList . snd . strSplit "="

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
  let lns = lines $ strReplace ">" "]" $ strReplace "<" "[" contents
  let info = map (map parseGeneral . strSplitAll ", ") lns
  let accelerations = map last info
  let withIndexes = zip accelerations [0 ..]
  let maxAccel = minimizeWithFunc (manhattanDistance . fst) withIndexes
  print $ snd maxAccel

main :: IO ()
main =
  mainWork "input.txt"
