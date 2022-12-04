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
import           Data.Vector   hiding (map, (++))
import           System.IO

-- 10.12.20

execute :: Vector (String, Int) -> Int -> (Int, Int)
execute = go Set.empty 0 (negate 1)
  where
    go reached ip oldIP instructions accumulator =
      if Set.member ip reached
        then (accumulator, oldIP)
        else case instructions !? ip of
          Nothing -> (accumulator, oldIP)
          Just ("nop", _) -> go newReached (succ ip) ip instructions accumulator
          Just ("jmp", n) -> go newReached (ip + n) ip instructions accumulator
          Just ("acc", n) -> go newReached (succ ip) ip instructions (accumulator + n)
      where
        newReached = Set.insert ip reached

modifyOps :: Vector (String, Int) -> Int -> Vector (String, Int)
modifyOps v changeAt = v // [(changeAt, newVal)]
  where
    (oldOp, oldVal) = v ! changeAt
    newVal = (changeOp oldOp, oldVal)
    changeOp "jmp" = "nop"
    changeOp "nop" = "jmp"
    changeOp op    = op

readInt' :: String -> Int
readInt' []         = 0
readInt' ('+' : xs) = read xs
readInt' xs         = read xs

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> Int -> IO ()
mainWork filename accumulator = do
  contents <- readFile filename
  let ops = fromList $ map (mapPair id readInt' . strSplit " ") $ lines contents
  let res = execute ops accumulator
  print $ fst res
  print res
  let newOps = modifyOps ops $ snd res
  let newRes = execute newOps accumulator
  print $ fst newRes
  print newRes

main :: IO ()
main =
  mainWork "test.txt" 0
