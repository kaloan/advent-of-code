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
import           Data.Foldable (toList)
import           Data.List     (foldl', sort)
import qualified Data.Map      as Map
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 14.12.21
completeInstructions ::
  [((String, Int -> Int -> Int, Int), (String, Int -> Int -> Bool, Int))] ->
  (Map.Map String Int, Int)
completeInstructions = go 0 Map.empty
  where
    go m calculated [] = (calculated, m)
    go m calculated (((register, func, change), (checkRegister, checkOp, checkValue)) : instructions) =
      case Map.lookup checkRegister calculated of
        Nothing
          | checkOp 0 checkValue -> go newMax (Map.insert register newValue calculated) instructions
          | otherwise -> go newMax calculated instructions
        Just val
          | checkOp val checkValue -> go newMax (Map.insert register newValue calculated) instructions
          | otherwise -> go newMax calculated instructions
      where
        newValue = func change $ Map.findWithDefault 0 register calculated
        newMax = max m newValue

parseAction :: String -> (String, Int -> Int -> Int, Int)
parseAction = go . strSplitAll " "
  where
    go [register, func, num] = (register, parseFunc func, readInt num)
    parseFunc "inc"  = (+)
    parseFunc "dec"  = flip (-)
    parseFunc "mult" = (*)
    parseFunc "div"  = flip div

parseCondition :: String -> (String, Int -> Int -> Bool, Int)
parseCondition = go . strSplitAll " "
  where
    go [register, check, num] = (register, parseCheck check, readInt num)
    parseCheck "==" = (==)
    parseCheck "!=" = (/=)
    parseCheck "<=" = (<=)
    parseCheck "<"  = (<)
    parseCheck ">=" = (>=)
    parseCheck ">"  = (>)

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

revPair :: (a, b) -> (b, a)
revPair (x, y) = (y, x)

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let actionConditions' = map (strSplit " if ") $ lines contents
  let actionConditions = map (mapPair parseAction parseCondition) actionConditions'
  let endingState = completeInstructions actionConditions
  let registerValues = reverse $ sort $ map revPair $ Map.assocs $ fst endingState
  let largestValue = fst $ head registerValues
  print largestValue
  let largestValueEver = snd endingState
  print largestValueEver

main :: IO ()
main =
  mainWork "input.txt"
