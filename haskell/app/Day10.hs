module Main where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Set (Set, union, (\\))
import qualified Data.Set as S

data Role = Open | Closed deriving (Show, Eq)

data Type = Circle | Square | Curly | Triangle deriving (Show, Eq)

data Bracket = Bracket Role Type deriving (Show, Eq)

parse input = fmap parseChar <$> lines input
  where
    parseChar char =
      case char of
        '(' -> Bracket Open Circle
        '[' -> Bracket Open Square
        '{' -> Bracket Open Curly
        '<' -> Bracket Open Triangle
        ')' -> Bracket Closed Circle
        ']' -> Bracket Closed Square
        '}' -> Bracket Closed Curly
        '>' -> Bracket Closed Triangle
        _ -> error $ "Unknown bracket! " ++ [char]

score :: Type -> Integer
score t =
  case t of
    Circle -> 3
    Square -> 57
    Curly -> 1197
    Triangle -> 25137

findIllegalChar row = go row []
  where
    go :: [Bracket] -> [Bracket] -> Maybe Bracket
    go [] _ = Nothing
    go (x@(Bracket Open _) : xs) stack = go xs (x : stack)
    go (x@(Bracket Closed t) : xs) ((Bracket _ t') : ys) =
      if t == t'
        then go xs ys
        else Just x

part1 input =
  let rows = parse input
   in map findIllegalChar rows

main :: IO ()
main = do
  example <- readFile "../input/day10.example"
  real <- readFile "../input/day10.real"

  putStrLn $ "part1 example: " <> show (part1 example)

-- putStrLn $ "part1 real: " <> show (part1 real)
-- putStrLn $ "part2 example: " <> show (part2 example)
-- putStrLn $ "part2 real: " <> show (part2 real)