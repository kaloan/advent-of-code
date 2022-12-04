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
import           Data.Char       (isLetter, toUpper)
import           Data.List       (find, foldl', sort)
import           Data.Map.Strict hiding (foldl', map)
import           Data.Maybe      (fromJust)
import           Data.Strings    (strSplit, strSplitAll)
import           System.IO

-- 24.12.21
type Register = Char

type ModelNumber = [Int]

data Second = Register Register | Num Int deriving (Read, Show)

data Instruction
  = Inp Register
  | Add Register Second
  | Mul Register Second
  | Div Register Second
  | Mod Register Second
  | Eql Register Second
  deriving (Read, Show)

isInp :: Instruction -> Bool
isInp (Inp _) = True
isInp _       = False

boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0

getSecond :: Instruction -> (Register, Second)
getSecond (Inp _)          = undefined
getSecond (Add reg second) = (reg, second)
getSecond (Mul reg second) = (reg, second)
getSecond (Div reg second) = (reg, second)
getSecond (Mod reg second) = (reg, second)
getSecond (Eql reg second) = (reg, second)

opToFunc :: Instruction -> (Int -> Int -> Int)
opToFunc (Inp _)   = \_ x -> x
opToFunc (Add _ _) = (+)
opToFunc (Mul _ _) = (*)
opToFunc (Div _ _) = div
opToFunc (Mod _ _) = mod
-- opToFunc (Eql _ _)  = boolToInt . (==)
opToFunc (Eql _ _) = \x y -> boolToInt $ x == y

calculate :: Instruction -> Maybe Int -> Map Register Int -> Map Register Int
calculate = undefined

execute :: [Instruction] -> ModelNumber
execute program =
  reverse $ snd $ go [] program initRegisters
  where
    go :: ModelNumber -> [Instruction] -> Map Register Int -> (Bool, ModelNumber)
    go given [] regs =
      (regs ! 'z' == 0, given)
    go given ((Inp reg) : program) regs =
      case find fst [go (n : given) program (insert reg n regs) | n <- [9 .. 1]] of
        Nothing             -> (False, given)
        Just (True, proper) -> (True, proper)
    go given (instruction : program) regs =
      case getSecond instruction of
        (reg, Num n) ->
          go given program (insertWith (opToFunc instruction) reg n regs)
        (reg, Register otherReg) ->
          go given program (insertWith (opToFunc instruction) reg (regs ! reg) regs)

registers :: [Register]
registers = ['x', 'y', 'z', 'w']

initRegisters :: Map Register Int
initRegisters = foldl' (\acc reg -> insert reg 0 acc) empty registers

readArithmetic :: String -> Instruction
readArithmetic = read

parseInstruction :: String -> Instruction
parseInstruction = go . strSplitAll " "
  where
    go ["inp", regStr] = Inp (head regStr)
    go [inst, regStr, secondStr] =
      readArithmetic (titleCase inst ++ " '" ++ regStr ++ "' (" ++ sec ++ ")")
      where
        sec =
          if isLetter (head secondStr)
            then "Register '" ++ secondStr ++ "'"
            else "Num " ++ secondStr

titleCase :: String -> String
titleCase []       = []
titleCase (x : xs) = toUpper x : xs

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
  let program = map parseInstruction lns
  print program
  print $ execute program

main :: IO ()
main =
  mainWork "input.txt"
