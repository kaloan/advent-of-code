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
import           Data.List     (foldl', partition)
import           System.IO

-- 03.12.21

mapPair :: (a -> c) -> (b -> d) -> (a, b) -> (c, d)
mapPair f g (x, y) = (f x, g y)

addPair :: (Num a) => (a, a) -> a
addPair = uncurry (+)

data Bit = Zero | One
  deriving (Show, Eq)

intToBit :: Integer -> Bit
intToBit 0 = Zero
intToBit 1 = One
intToBit _ = error "Cannot transform non bit Integer to type Bit"

bitToInt :: Bit -> Integer
bitToInt Zero = 0
bitToInt One  = 1

type Binary = [Bit]

binaryInIntToBinary :: [Integer] -> Binary
binaryInIntToBinary = map intToBit

binaryToInt :: Binary -> Integer
binaryToInt = foldl go 0
  where
    --go = uncurry . curry addPair . curry $ mapPair (*2) (bitToInt)
    go acc Zero = 2 * acc
    go acc One  = 1 + 2 * acc

binaryComplement :: Binary -> Binary
binaryComplement = map go
  where
    go Zero = One
    go One  = Zero

transpose :: [[a]] -> [[a]]
transpose []       = []
transpose ([] : _) = []
transpose xs       = map head xs : transpose (map tail xs)

higherOccurencesInRow :: Binary -> Bit
higherOccurencesInRow l = if zeros > ones then Zero else One
  where
    (zeros, ones) = foldr go (0, 0) l
    go Zero = mapPair (+ 1) id
    go One  = mapPair id (+ 1)

mostInColumns :: [Binary] -> Binary
mostInColumns lss = go [] $ reverse $ transpose lss
  where
    go mostBits []         = mostBits
    go mostBits (xs : xss) = go (higherOccurencesInRow xs : mostBits) xss

powerConsumption :: [Binary] -> Integer
powerConsumption lss = gamma * epsilon
  where
    higherBits = mostInColumns lss
    gamma = binaryToInt higherBits
    epsilon = binaryToInt $ binaryComplement higherBits

lifeSupportRating :: [Binary] -> ([Binary] -> Binary) -> Integer
lifeSupportRating lss getNextBin = go lss [] $ getNextBin lss
  where
    go [] _ _ = error "Reached empty list when calculating lifeSupportRating"
    go [bin] revTillNow _ = binaryToInt $ reverse revTillNow ++ bin
    go xss revTillNow (m : _) = go nextSearch (m : revTillNow) nextBin
      where
        nextSearch = map tail $ filter ((== m) . head) xss
        nextBin = getNextBin nextSearch
    go _ _ _ = error "Some error when calculating lifeSupportRating"

oxygenGeneratorRating :: [Binary] -> Integer
oxygenGeneratorRating lss = lifeSupportRating lss mostInColumns

co2ScrubberRating :: [Binary] -> Integer
co2ScrubberRating lss = lifeSupportRating lss (binaryComplement . mostInColumns)

readInt :: String -> Int
readInt = read

readBinary :: String -> Binary
readBinary = map go
  where
    go '0' = Zero
    go '1' = One
    go _   = error "Bad format"

readInteger :: String -> Integer
readInteger = read

mainWork :: FilePath -> IO ()
mainWork filename = do
  contents <- readFile filename
  let diagnosticReport = map readBinary $ lines contents
  let powCons = powerConsumption diagnosticReport
  let oGR = oxygenGeneratorRating diagnosticReport
  let co2SR = co2ScrubberRating diagnosticReport
  print powCons
  print $ oGR * co2SR

main :: IO ()
main = do
  mainWork "input.txt"
