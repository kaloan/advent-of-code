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
import           Data.List        (delete, foldl', sort)
import           Data.List.Unique
import           Data.Matrix
import           Data.Maybe
import           Data.Strings     (strSplit, strSplitAll)
import           System.IO

-- 02.12.20
xor :: Bool -> Bool -> Bool
xor x y = (x && not y) || (not x && y)

parseLetterRequirement :: String -> (Char, Int, Int)
parseLetterRequirement l = do
  let (toParseNums, toParseLetter) = strSplit " " l
  let letter = head toParseLetter
  let (toParseMin, toParseMax) = strSplit "-" toParseNums
  (letter, readInt toParseMin, readInt toParseMax)

validatePassword :: (Ord a, Eq a) => (a, Int, Int) -> [a] -> Bool
validatePassword (c, minC, maxC) l = cnt >= minC && cnt <= maxC
  where
    cnt = countElem c l

validatePassword' :: (Ord a, Eq a) => (a, Int, Int) -> [a] -> Bool
validatePassword' (c, left, right) l = xor ((l !! pred left) == c) ((l !! pred right) == c)

-- validatePassword (c, minC, maxC) l = go $ occurrences l
--   where
--     go ll = go' $ countElem c ll
--     --go ll = go' $ fromMaybe 0 $ lookup c ll
--     go' x = x >= minC && x <= maxC

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
  let parseReqAndPass = map (mapPair parseLetterRequirement id . strSplit ": ") lns
  print $ length $ filter (uncurry validatePassword) parseReqAndPass
  print $ length $ filter (uncurry validatePassword') parseReqAndPass

main :: IO ()
main =
  mainWork "input.txt"
