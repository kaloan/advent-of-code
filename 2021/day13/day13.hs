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
import           Data.List        (foldl', sort)
import           Data.List.Unique
import           Data.Matrix
import           Data.Strings     (strSplit, strSplitAll)
import           System.IO

-- 13.12.21
foldAlong :: (Num a, Ord a) => ((a, a) -> a) -> ((a, a) -> a -> (a, a)) -> a -> [(a, a)] -> [(a, a)]
foldAlong f g about = foldl' go []
  where
    go acc x
      | around > about = g x (about + about - around) : acc
      | around < about = g x around : acc
      | otherwise = acc
      where
        around = f x

foldTrue :: (Num a, Ord a) => (Char, a) -> [(a, a)] -> [(a, a)]
foldTrue ('x', line) = uniq . sort . foldAlong fst flipX line
foldTrue ('y', line) = uniq . sort . foldAlong snd flipY line
foldTrue _           = undefined

flipX :: (a, b) -> a -> (a, b)
flipX (_, y) newX = (newX, y)

flipY :: (a, b) -> b -> (a, b)
flipY (x, _) newY = (x, newY)

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
  let (points', folds') = strSplit "\n\n" contents
  let points = map (mapPair readInt readInt . strSplit ",") $ lines points'
  let folds = map (mapPair head readInt . strSplit "=" . (head . tail . tail . words)) (lines folds')
  print $ length $ foldTrue (head folds) points
  let endCoords = foldl' (\acc dir -> foldTrue dir acc) points folds
  let cols = succ $ maximum $ map fst endCoords
  let rows = succ $ maximum $ map snd endCoords
  let matrixStart = zero rows cols
  print $ foldl' (\acc (x, y) -> setElem 11 (succ y, succ x) acc) matrixStart endCoords

main :: IO ()
main =
  mainWork "input.txt"
