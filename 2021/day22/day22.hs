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
import           Data.List           (foldl', sort)
import           Data.Set            hiding (foldl', map)
import           Data.Strings        (strSplit, strSplitAll)
import           Language.Haskell.TH
import           System.IO
import           Text.Scanf

-- 22.12.21
--cubeList :: ((Int, Int), (Int, Int), (Int, Int)) -> [(Int, Int, Int)]
cubeList :: (Enum a) => ((a, a), (a, a), (a, a)) -> [(a, a, a)]
cubeList ((xmin, xmax), (ymin, ymax), (zmin, zmax)) =
  [(x, y, z) | x <- [xmin .. xmax], y <- [ymin .. ymax], z <- [zmin .. zmax]]

turnOn :: (Enum a, Ord a) => ((a, a), (a, a), (a, a)) -> Set (a, a, a) -> Set (a, a, a)
turnOn coords = union (fromList $ cubeList coords)

turnOff :: (Enum a, Ord a) => ((a, a), (a, a), (a, a)) -> Set (a, a, a) -> Set (a, a, a)
turnOff coords s = difference s (fromList $ cubeList coords)

reboot :: (Enum a, Ord a) => [(String, ((a, a), (a, a), (a, a)))] -> Set (a, a, a)
reboot = foldl' go empty
  where
    go acc ("on", coords)  = turnOn coords acc
    go acc ("off", coords) = turnOff coords acc

-- filterCoords :: (Ord a) => ((a, a), (a, a), (a, a)) -> a -> a -> Bool
-- filterCoords ((xmin, xmax), (ymin, ymax), (zmin, zmax)) minCoords maxCoords =
--   checkCoord (xmin, xmax) && checkCoord (xmin, xmax) && checkCoord (xmin, xmax)
--   where
--     checkCoord :: (Ord a) => (a, a) -> Bool
--     checkCoord (cMin, cMax)
--       | cMin < minCoords = False
--       | cMax > maxCoords = False
--       | otherwise = True

limitCoords :: (Ord a) => a -> a -> ((a, a), (a, a), (a, a)) -> ((a, a), (a, a), (a, a))
limitCoords minCoords maxCoords ((xmin, xmax), (ymin, ymax), (zmin, zmax)) =
  ( (max xmin minCoords, min xmax maxCoords),
    (max ymin minCoords, min ymax maxCoords),
    (max zmin minCoords, min zmax maxCoords)
  )

parseCoords :: String -> ((Int, Int), (Int, Int), (Int, Int))
parseCoords = go . scanf scanStr
  where
    --scanStr = [fmt|x=%d..%d,y=%d..%d,z=%d..%d|]
    scanStr =
      fmt_ ("x=" % int . ".." % int . ",y=" % int . ".." % int . ",z=" % int . ".." % int)
    go (Just (xmin :+ xmax :+ ymin :+ ymax :+ zmin :+ zmax :+ ())) =
      ((xmin, xmax), (ymin, ymax), (zmin, zmax))

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

readInt :: String -> Int
readInt = read

readIntList :: String -> [Int]
readIntList = read

parseIntList :: String -> [Int]
parseIntList s = read $ "[" ++ s ++ "]"

mainWork :: FilePath -> Int -> Int -> IO ()
mainWork filename minCoord maxCoord = do
  contents <- readFile filename
  let lns = lines contents
  let parsed = map (mapPair id (limitCoords minCoord maxCoord . parseCoords) . strSplit " ") lns
  let res = reboot parsed
  print parsed
  print $ size res

main :: IO ()
main =
  mainWork "test1.txt" (negate 50) 50
