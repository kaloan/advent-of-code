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
mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

mapTree :: (a -> b) -> Tree a -> Tree b
-- mapTree f (Node {rootLabel = vertex, subForest = children}) =
--   Node {rootLabel = f vertex, subForest = map (mapTree f) children}
mapTree f (Node vertex children) =
  Node (f vertex) (map (mapTree f) children)

parseHead :: String -> (String, Int)
parseHead = go . strSplit " ("
  where
    go (vertex, weightWithBracket) = (vertex, readInt $ init weightWithBracket)

constructEdge :: ((String, Int), [String]) -> ((String, Int), String, [String])
constructEdge ((vertex, weight), linkedVerteces) = ((vertex, weight), vertex, linkedVerteces)

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

parseStringList :: String -> [String]
--parseStringList s = read $ "[" ++ s ++ "]"
parseStringList [] = []
parseStringList s  = strSplitAll ", " s

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let lns = lines contents
  let withWeight = map (mapPair parseHead parseStringList . strSplit " -> ") lns
  --print withWeight
  let programEdges = map constructEdge withWeight
  let (programGraph, nodeFromVertex, vertexFromKey) = graphFromEdges programEdges
  let topoSorted = topSort programGraph
  let bottomProgram = head topoSorted
  print $ fst3 $ nodeFromVertex bottomProgram
  let treeWithBottomRoot = head $ dfs programGraph [bottomProgram]
  print treeWithBottomRoot
  print $ fmap (fst3 . nodeFromVertex) treeWithBottomRoot

main :: IO ()
main =
  mainWork "test.txt"
