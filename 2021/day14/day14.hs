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

-- 14.12.21
polymerise :: (Eq a) => Seq a -> [([a], a)] -> Seq a
polymerise = go Empty
  where
    go :: (Eq a) => Seq a -> Seq a -> [([a], a)] -> Seq a
    go acc Empty _ = acc
    go acc (x :<| Empty) _ = acc |> x
    go acc (x :<| y :<| xs) rules =
      case lookup [x, y] rules of
        Just c  -> go (acc |> x |> c) (y :<| xs) rules
        Nothing -> go (acc |> x) (y :<| xs) rules

constructMap :: (Ord a) => [a] -> Map.Map (a, a) Integer
constructMap = go Map.empty
  where
    go m []           = m
    go m [_]          = m
    go m (x : y : xs) = go (Map.insertWith (+) (x, y) 1 m) (y : xs)

decreaseOfStartAndEnd :: (Ord a) => a -> a -> Map.Map a Integer -> Map.Map a Integer
decreaseOfStartAndEnd s e mapped = Map.insertWith ensureOne e 1 (Map.insertWith ensureOne s 1 mapped)
  where
    -- In the rare case the final ones don't appear anywhere else
    ensureOne 1 1   = 1
    ensureOne 1 old = old - 1

pairToCharCounts :: (Ord a) => Map.Map (a, a) Integer -> Map.Map a Integer
pairToCharCounts m = go Map.empty $ Map.toList m
  where
    go final [] = final
    go counts (((x, y), cnt) : pairCounts) =
      go (Map.insertWith (+) x cnt (Map.insertWith (+) y cnt counts)) pairCounts

mapPolymerise :: (Ord a) => Map.Map (a, a) Integer -> Map.Map (a, a) a -> Map.Map (a, a) Integer
mapPolymerise m = go Map.empty (Map.assocs m)
  where
    go res [] _ = res
    go res (((x, y), cnt) : pairCounts) rules =
      case Map.lookup (x, y) rules of
        Nothing -> go (Map.insert (x, y) cnt res) pairCounts rules
        Just c -> go (Map.insertWith (+) (x, c) cnt (Map.insertWith (+) (c, y) cnt res)) pairCounts rules

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

-- Works fine for small inputs and short time
-- Otherwise you do O(polymerLength) work for one insertion
mainWork :: FilePath -> Integer -> IO ()
mainWork filename iters = do
  contents <- readFile filename
  let (initPolymer', rules') = strSplit "\n\n" contents
  let initPolymer = fromList initPolymer'
  let rules = map (mapPair id head . strSplit " -> ") (lines rules')
  --print $ initPolymer
  --print $ rules
  let finalPolymer = toList $ foldl' (\polymer _ -> polymerise polymer rules) initPolymer [1 .. iters]
  let counts = occurrences finalPolymer
  let maxDiff = fst (last counts) - fst (head counts)
  print counts
  print maxDiff

succAtLetter :: (Integral a, Eq b) => b -> (a, b) -> (a, b)
succAtLetter x (cnt, letter) =
  if letter == x
    then (cnt + 1, letter)
    else (cnt, letter)

-- Now we have to do O(typesOfPairs*(log(typesOfPairs) + log(numberOfRules))) work for one insertion
-- typesOfPairs because we do a simple recursion/iteration over the list and for each one we:
-- lookup for a rule (the second summand)
-- push to the current map which in the final pair would have size typesOfPairs (the first summand)
-- Notice that the maximumTypesOfPairs and numberOrRules are actually finite in the whole program, unlike
-- the previous algorithm where polymerLength increases after each insertion
mainWork' :: FilePath -> Integer -> IO ()
mainWork' filename iters = do
  contents <- readFile filename
  let (initPolymer', rules') = strSplit "\n\n" contents
  let rules = Map.fromList $ map (mapPair (\str -> (head str, last str)) head . strSplit " -> ") (lines rules')
  let initPolymer = constructMap initPolymer'
  --print rules
  let finalPolymer = foldl' (\polymer _ -> mapPolymerise polymer rules) initPolymer [1 .. iters]
  let finalCounts'' = Map.toList $ pairToCharCounts finalPolymer
  let finalCounts' = map (\(letter, cnt) -> (div cnt 2, letter)) finalCounts''
  -- Most of the resulting elements would be in 2 pairs and as such pairToCharCounts
  -- would give us twice their ammount. However the elements in starting and ending
  -- positions will never be counted twice and so we need to divide only the number of the
  -- ones in the interrior
  let finalCounts = map (succAtLetter (last initPolymer') . succAtLetter (head initPolymer')) finalCounts'
  let sortedFinalCounts = Data.List.sort finalCounts
  print sortedFinalCounts
  let maxDiff = fst (last sortedFinalCounts) - fst (head sortedFinalCounts)
  print maxDiff

main :: IO ()
main =
  --mainWork "test.txt" 40
  mainWork' "input.txt" 40
