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
import           Data.List        (find, foldl', sort)
import           Data.List.Unique
import qualified Data.Map         as Map
import           Data.Sequence    hiding (lookup)
import           Data.Strings     (strReplace, strSplit, strSplitAll)
import           System.IO

-- 18.12.21
data Pair a = E a | P (Pair a, Pair a) deriving (Show, Read, Eq)

magnitude :: (Num a) => Pair a -> a
magnitude (E x)             = x
magnitude (P (left, right)) = 3 * magnitude left + 2 * magnitude right

split :: (Num a, Integral a) => Pair a -> (Pair a, Bool)
split (E n) = do
  let (l, m) = divMod n 2
  if n > 10
    then (P (E l, E $ l + m), True)
    else (E n, False)
split (P (left, right)) = do
  let (newLeft, leftChanged) = split left
  let (newRight, rightChanged) = split right
  if leftChanged
    then (P (newLeft, right), True)
    else (P (left, newRight), rightChanged)

addToLeftMost :: (Num a) => Pair a -> a -> Pair a
addToLeftMost (E x) y      = E $ x + y
addToLeftMost (P (u, v)) y = P (addToLeftMost u y, v)

addToRightMost :: (Num a) => Pair a -> a -> Pair a
addToRightMost (E x) y      = E $ x + y
addToRightMost (P (u, v)) y = P (u, addToRightMost v y)

explode :: (Num a) => Pair a -> (Pair a, Bool)
-- Leftmost
explode (P (P (P (P (P (E _, E y), r1), r2), r3), r4)) =
  (P (P (P (P (E 0, addToLeftMost r1 y), r2), r3), r4), True)
-- Rightmost
explode (P (l4, P (l3, P (l2, P (l1, P (E x, E _)))))) =
  (P (l4, P (l3, P (l2, P (addToRightMost l1 x, E 0)))), True)
-- One right of leftmost
explode (P (P (P (P (l1, P (E x, E y)), r1), r2), r3)) =
  (P (P (P (P (addToRightMost l1 x, E 0), addToLeftMost r1 y), r2), r3), True)
-- One left of rightmost
explode (P (l3, P (l2, P (l1, P (P (E x, E y), r1))))) =
  (P (l3, P (l2, P (addToRightMost l1 x, P (E 0, addToLeftMost r1 y)))), True)
-- With one right outside
explode (P (P (l3, P (l2, P (l1, P (E x, E y)))), r1)) =
  (P (P (l3, P (l2, P (addToRightMost l1 x, E 0))), addToLeftMost r1 y), True)
explode (P (P (l2, P (l1, P (P (E x, E y), r1))), r2)) =
  (P (P (l2, P (addToRightMost l1 x, P (E 0, addToLeftMost r1 y))), r2), True)
-- With one left outside
explode (P (l1, P (P (P (P (E x, E y), r1), r2), r3))) =
  (P (addToRightMost l1 x, P (P (P (E 0, addToLeftMost r1 y), r2), r3)), True)
explode (P (l2, P (P (P (l1, P (E x, E y)), r1), r2))) =
  (P (l2, P (P (P (addToRightMost l1 x, E 0), addToLeftMost r1 y), r2)), True)
-- When there aren't 5 nested pairs
explode x = (x, False)

reduce :: (Num a, Integral a) => Pair a -> Pair a
reduce x
  | snd exploded = reduce $ fst exploded
  | snd splitted = reduce $ fst splitted
  | otherwise = x
  where
    exploded = explode x
    splitted = split x

addP :: (Num a, Integral a) => Pair a -> Pair a -> Pair a
--addP = curry P
addP x y = reduce $ P (x, y)

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

parsePair :: String -> Pair Int
parsePair = read . transformToPair

transformToPair :: String -> String
transformToPair = go . strReplace "]" ")" . strReplace "[" "P ("
  where
    parseDigit :: Char -> Maybe Integer
    parseDigit c =
      toInteger <$> find (\x -> toEnum (x + fromEnum '0') == c) [0 .. 9]
    go [] = []
    go (c : str) =
      case parseDigit c of
        Nothing -> c : go str
        Just _  -> 'E' : ' ' : c : go str

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let pairs = map parsePair $ lines contents
  -- let testSplit = P (P (P (P (E 0, E 7), E 4), P (E 15, P (E 0, E 13))), P (E 1, E 1))
  -- print $ split $ fst $ split testSplit
  --print pairs
  -- print $ explode $ head pairs
  let res = foldl' addP (head pairs) (tail pairs)
  print res
  print $ magnitude res

main :: IO ()
main =
  mainWork "test4.txt"
