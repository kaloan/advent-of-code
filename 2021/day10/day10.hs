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
import           Data.List     (foldl', sort)
import           Data.Strings  (strSplit, strSplitAll)
import           System.IO

-- 10.12.21

data IncompleteOrValWrong = Incomplete String | Wrong Integer
  deriving (Show)

isIncomplete :: IncompleteOrValWrong -> Bool
isIncomplete (Incomplete _) = True
isIncomplete _              = False

getIncomplete :: IncompleteOrValWrong -> String
getIncomplete (Incomplete x) = x
getIncomplete _              = undefined

getValWrong :: IncompleteOrValWrong -> Integer
getValWrong (Wrong x) = x
getValWrong _         = undefined

isOpeningBrace :: Char -> Bool
isOpeningBrace x = x == '(' || x == '{' || x == '[' || x == '<'

matchingPair :: Char -> Char -> Bool
matchingPair '(' ')' = True
matchingPair '[' ']' = True
matchingPair '{' '}' = True
matchingPair '<' '>' = True
matchingPair _ _     = False

getMatchingClosingPair :: Char -> Char
getMatchingClosingPair '(' = ')'
getMatchingClosingPair '[' = ']'
getMatchingClosingPair '{' = '}'
getMatchingClosingPair '<' = '>'

wrongBraceVal :: Char -> Integer
wrongBraceVal ')' = 3
wrongBraceVal ']' = 57
wrongBraceVal '}' = 1197
wrongBraceVal '>' = 25137

incompleteBraceVal :: Char -> Integer
incompleteBraceVal ')' = 1
incompleteBraceVal ']' = 2
incompleteBraceVal '}' = 3
incompleteBraceVal '>' = 4

incompleteStepMultiplier :: Integer
incompleteStepMultiplier = 5

checkLine :: String -> IncompleteOrValWrong
checkLine = go []
  where
    go acc [] = Incomplete acc
    go [] (x : xs) = go [x] xs
    go acc@(opened : openeds) (x : xs)
      | isOpeningBrace x = go (x : acc) xs
      | matchingPair opened x = go openeds xs
      | otherwise = Wrong $ wrongBraceVal x

leftToCompleteScore :: String -> Integer
leftToCompleteScore = foldl' go 0
  where
    go acc x = incompleteStepMultiplier * acc + incompleteBraceVal x

getMedian :: (Integral a) => [a] -> a
getMedian xs
  | odd len = xs !! (len `div` 2)
  | otherwise = (xs !! (len `div` 2) + xs !! (len `div` 2 - 1)) `div` 2
  where
    len = length xs

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let directions = lines contents
  let parseDirections = map checkLine directions
  --let corrupted = filter (not . isIncomplete) parseDirections
  --print $ sum $ map getValWrong corrupted
  let incomplete = filter isIncomplete parseDirections
  let leftToComplete = map (map getMatchingClosingPair . getIncomplete) incomplete
  let leftToCompleteScores = sort $ map leftToCompleteScore leftToComplete
  print $ getMedian leftToCompleteScores

main :: IO ()
main = do
  mainWork "input.txt"
