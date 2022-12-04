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
import           Data.List     (delete, foldl', sort)
import           Data.Maybe
import qualified Data.Set      as Set hiding (delete)
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 01.12.20
getConjugates :: (Num a, Ord a) => [a] -> a -> Maybe (a, a)
getConjugates l m = listToMaybe conjugates
  where
    hashed = Set.fromList l
    conjugates = foldl' go [] l
    go acc x = if Set.member (m - x) hashed then (x, m - x) : acc else acc

getTripleConjugates :: (Num a, Ord a) => [a] -> a -> [(a, a, a)]
getTripleConjugates l m = mapMaybe (\x -> takeExisting x $ go x) l
  where
    go x = getConjugates (delete x l) (m - x)
    ifExists x (y, z) = (x, y, z)
    takeExisting x = fmap (ifExists x)

multTriple :: (Num a) => (a, a, a) -> a
multTriple (x, y, z) = x * y * z

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> Int -> IO ()
mainWork filename sumTo = do
  contents <- readFile filename
  let nums = map readInt $ lines contents
  let (n, m) = fromJust $ getConjugates nums sumTo
  print $ n * m
  print $ multTriple $ head $ getTripleConjugates nums sumTo

main :: IO ()
main =
  mainWork "input.txt" 2020
