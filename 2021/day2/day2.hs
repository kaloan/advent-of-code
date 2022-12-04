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
import           System.IO

-- 02.12.21

multPair :: (Num a) => (a, a) -> a
multPair = uncurry (*)

data Instruction a
  = Up a
  | Down a
  | Forward a
  deriving (Show)

finalPosition :: (Num a) => [Instruction a] -> (a, a)
finalPosition = foldr go (0, 0)
  where
    go (Up x) (h, d)      = (h, d - x)
    go (Down x) (h, d)    = (h, d + x)
    go (Forward x) (h, d) = (h + x, d)

finalPosition' :: (Num a) => [Instruction a] -> (a, a)
finalPosition' = fst . foldl go ((0, 0), 0)
  where
    go ((h, d), aim) (Up x)      = ((h, d), aim - x)
    go ((h, d), aim) (Down x)    = ((h, d), aim + x)
    go ((h, d), aim) (Forward x) = ((h + x, d + aim * x), aim)

day2Task :: (Num a) => [Instruction a] -> a
day2Task = multPair . finalPosition

day2Task' :: (Num a) => [Instruction a] -> a
day2Task' = multPair . finalPosition'

readInt :: String -> Int
readInt = read

readInstruction :: [String] -> Instruction Int
readInstruction ["up", x]      = Up $ read x
readInstruction ["down", x]    = Down $ read x
readInstruction ["forward", x] = Forward $ read x
readInstruction _              = error "Instruction list non formatted"

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let instructions = map (readInstruction . words) $ lines contents
  print $ day2Task instructions
  print $ day2Task' instructions

main :: IO ()
main = do
  mainWork "input.txt"
