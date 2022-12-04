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
import           Data.List     (foldl')
import           Data.Matrix
import           Data.Strings  (strSplit)
import           System.IO

-- 05.12.21

type Point = (Int, Int)

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

applyPair :: (a1 -> a2 -> c) -> (b1 -> b2 -> d) -> (a1, b1) -> (a2, b2) -> (c, d)
applyPair f g (x, y) (w, t) = (f x w, g y t)

listToPair :: [a] -> (a, a)
listToPair [x, y] = (x, y)
listToPair _ = error "The provided list to listToPair does not have 2 elements"

getBoundaries :: [(Point, Point)] -> (Point, Point)
getBoundaries = foldl' go ((0, 0), (0, 0))
  where
    go ((xMin, yMin), (xMax, yMax)) ((x1, y1), (x2, y2)) =
      ((minimum xCoords, minimum yCoords), (maximum xCoords, maximum yCoords))
      where
        xCoords = [x1, x2, xMin, xMax]
        yCoords = [y1, y2, yMin, yMax]

translateAll :: [(Point, Point)] -> Point -> [(Point, Point)]
translateAll linesList toBecomeOrigin = map (mapPair translate translate) linesList
  where
    translate x = applyPair (-) (-) x toBecomeOrigin

integerPointsInLine :: (Point, Point) -> [Point]
integerPointsInLine (left@(xMin, yMin), right@(xMax, yMax))
  | left == right = [left]
  | xMin == xMax = [(xMin, y) | y <- [min yMin yMax .. max yMin yMax]]
  | yMin == yMax = [(x, yMin) | x <- [min xMin xMax .. max xMin xMax]]
  | otherwise = [] --go left [right]
  where
    protoStepX = xMax - xMin
    protoStepY = yMax - yMin
    toDivBy = gcd protoStepX protoStepY
    stepX = div protoStepX toDivBy
    stepY = div protoStepY toDivBy
    go (pX, pY) curr@((ppX, ppY) : xs) =
      if (ppX, ppY) == (pX, pY)
        then (pX, pY) : xs
        else go (pX, pY) $ (ppX - stepX, ppY - stepY) : curr
    go _ [] = undefined

--go (pX, pY) curr@((ppX, ppY) : xs) = go (pX, pY) $ (ppX - stepX, ppY - stepY) : curr

fillMatrix :: [(Point, Point)] -> Matrix Int -> Matrix Int
fillMatrix linesList = go (map integerPointsInLine linesList)
  where
    go [] m         = m
    go (ps : pss) m = go pss (go' ps m)
    go' [] m = m
    go' ((x, y) : ps) m = go' ps (setElem (1 + atXY) (x + 1, y + 1) m)
      where
        atXY = getElem (x + 1) (y + 1) m

countMatrixMoreThanX :: (Ord a, Num a) => Matrix a -> a -> Int
countMatrixMoreThanX matrix x = foldl' (\acc row -> acc + length (filter (> x) row)) 0 (toLists matrix)

readInt :: String -> Int
readInt = read

deleteByIndex :: Integer -> [a] -> [a]
deleteByIndex n l
  | n < 0 = error "Can't remove negative index"
  | otherwise = go l n []
  where
    go [] _ _ = []
    go (x : xs) m acc =
      if m == 0 then reverse acc ++ xs else go xs (pred m) (x : acc)

turnLineToPairOfCoords :: String -> (Point, Point)
turnLineToPairOfCoords =
  listToPair
    . map (mapPair readInt readInt . strSplit ",")
    . deleteByIndex 1
    . words

mainWork :: String -> IO ()
mainWork filename = do
  contents <- readFile filename
  let origPairsOfCoords = map turnLineToPairOfCoords $ lines contents
  let boundaries = getBoundaries origPairsOfCoords
  let pairsOfCoords = translateAll origPairsOfCoords (fst boundaries)
  let matrix = uncurry zero (mapPair (+ 1) (+ 1) $ applyPair (-) (-) (snd boundaries) (fst boundaries))
  let filledMatrix = fillMatrix pairsOfCoords matrix
  --print filledMatrix
  print $ countMatrixMoreThanX filledMatrix 1
  print $ mapPair (+ 1) (+ 1) $ applyPair (-) (-) (snd boundaries) (fst boundaries)

main :: IO ()
main = do
  mainWork "input.txt"

--mainWork "test.txt"

-- test <- readFile "test.txt"
-- let origPairsOfCoordsTest = map turnLineToPairOfCoords $ lines test
-- let boundariesTest = getBoundaries pairsOfCoordsTest
-- print boundariesTest
-- let matrixTest = uncurry zero (mapPair (+ 1) (+ 1) $ applyPair (-) (-) (snd boundariesTest) (fst boundariesTest))
-- contents <- readFile "input.txt"
-- let origPairsOfCoords = map turnLineToPairOfCoords $ lines contents
-- let boundaries = getBoundaries origPairsOfCoords
-- let pairsOfCoords = translateAll origPairsOfCoords (fst boundaries)
-- let matrix = uncurry zero (mapPair (+ 1) (+ 1) $ applyPair (-) (-) (snd boundaries) (fst boundaries))
-- print $ mapPair (+ 1) (+ 1) $ applyPair (-) (-) (snd boundaries) (fst boundaries)
