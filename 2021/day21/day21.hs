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

-- 22.12.21
zeroStartToOneStart :: Int -> Int -> Int
zeroStartToOneStart 0 m = m
zeroStartToOneStart n _ = n

move :: (Int, Int) -> Int -> Int -> Bool -> (Int, Int)
move (f, s) n boardSize True  = ((f + n) `mod` boardSize, s)
move (f, s) n boardSize False = (f, (s + n) `mod` boardSize)

play :: (Int, Int) -> Int -> Int -> Int -> (Int, Int)
play (fstart, sstart) boardSize maxAcc diceSides = go fstart sstart 0 (0, 0)
  where
    go f s m (facc, sacc) = do
      let moves = zeroStartToOneStart (9 * m + 6) diceSides
      let firstPlayerMoves = even m
      let (fnew, snew) = move (f, s) moves boardSize firstPlayerMoves
      let (faccNew, saccNew) =
            if firstPlayerMoves
              then (facc + zeroStartToOneStart fnew boardSize, sacc)
              else (facc, sacc + zeroStartToOneStart snew boardSize)
      if facc >= maxAcc
        then (sacc, 3 * m)
        else
          if sacc >= maxAcc
            then (facc, 3 * m)
            else go fnew snew (succ m) (faccNew, saccNew)

diracPlay :: (Int, Int) -> Int -> Int -> Int -> (Int, Int)
diracPlay (fstart, sstart) boardSize maxAcc diceSides = go fstart sstart 0 (0, 0) (0, 0)
  where
    go f s m (facc, sacc) (fwins, swins)
      | facc >= maxAcc = (succ fwins, swins)
      | sacc >= maxAcc = (fwins, succ swins)
      | otherwise = foldl' go' (fwins, swins) [1 .. diceSides]
      where
        go' (fCurWins, sCurWins) n = do
          let firstPlayerMoves = even m
          let (fnew, snew) = move (f, s) n boardSize firstPlayerMoves
          let (faccNew, saccNew) =
                if firstPlayerMoves
                  then (facc + zeroStartToOneStart fnew boardSize, sacc)
                  else (facc, sacc + zeroStartToOneStart snew boardSize)
          go fnew snew (succ m) (faccNew, saccNew) (fCurWins, sCurWins)

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> Int -> Int -> IO ()
mainWork filename boardSize maxAcc = do
  contents <- readFile filename
  let lns = lines contents
  let parsed = map (readInt . head . tail . strSplitAll ": ") lns
  let firstPlayerStart = head parsed
  let secondPlayerStart = last parsed
  --let result = play (firstPlayerStart, secondPlayerStart) boardSize maxAcc 100
  --print result
  --print $ uncurry (*) result
  let diracResult = diracPlay (firstPlayerStart, secondPlayerStart) boardSize maxAcc 3
  print diracResult
  print $ uncurry max diracResult

main :: IO ()
main =
  --mainWork "input.txt" 10 1000
  mainWork "test.txt" 10 21
