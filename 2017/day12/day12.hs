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
import           Data.Graph
import           Data.List        (foldl', sort)
import           Data.List.Unique
import           Data.Strings     (strSplit, strSplitAll)
import           System.IO

-- 12.12.17
transformRule :: (Int, [Int]) -> [(Int, Int)]
transformRule (from, toList) = map (from,) toList

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
  let lns = lines contents
  let protoRules = map (mapPair readInt parseIntList . strSplit " <-> ") lns
  let rules = concatMap transformRule protoRules
  let maxNum = pred $ length lns
  let programGraph = buildG (0, maxNum) rules
  let inGroupOf0 = reachable programGraph 0
  -- Since the graph is undirected the components coincide with the strongly connected components
  let connectedComponents = scc programGraph
  print $ length inGroupOf0
  print $ length connectedComponents

main :: IO ()
main =
  mainWork "input.txt"
