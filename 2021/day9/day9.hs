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
import           Data.Char     (digitToInt)
import           Data.List     (foldl')
import           Data.Matrix
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 09.12.21

getLowest :: (Ord a) => Matrix a -> [(Int, Int)]
getLowest mat = getLowest' mat
  where
    rows = nrows mat
    cols = ncols mat
    minOfSurrounding (i, j) =
      foldl'
        ( \acc (dx, dy) ->
            case safeGet (i + dx) (j + dy) mat of
              Nothing       -> acc
              Just otherVal -> if otherVal < currVal then False else acc
        )
        True
        [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1]]
      where
        currVal = getElem i j mat
    getLowest' mat = foldl' go [] [(i, j) | i <- [1 .. rows], j <- [1 .. cols]]
    go acc coords =
      if minOfSurrounding coords
        then coords : acc
        else acc

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
  let rows = map (map digitToInt) lns
  let mat = fromLists rows
  let getLwst = getLowest mat
  let valsOfLowest = map (\(i, j) -> 1 + getElem i j mat) getLwst
  print getLwst
  print $ sum valsOfLowest

main :: IO ()
main = do
  mainWork "input.txt"
