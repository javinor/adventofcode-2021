module Main where

import Data.Either (lefts, rights)
import Data.List (foldl', genericLength, sort)

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

scoreIllegal :: Bracket -> Integer
scoreIllegal b =
  case b of
    Bracket _ Circle -> 3
    Bracket _ Square -> 57
    Bracket _ Curly -> 1197
    Bracket _ Triangle -> 25137

scoreCompletion :: [Bracket] -> Integer
scoreCompletion = foldl' ((+) . (* 5)) 0 . map score
  where
    score :: Bracket -> Integer
    score b = case b of
      Bracket _ Circle -> 1
      Bracket _ Square -> 2
      Bracket _ Curly -> 3
      Bracket _ Triangle -> 4

rowType row = go row []
  where
    go :: [Bracket] -> [Bracket] -> Either Bracket [Bracket]
    go [] stack = Right stack
    go (x@(Bracket Open _) : xs) stack = go xs (x : stack)
    go (x@(Bracket Closed _) : _) [] = Left x
    go (x@(Bracket Closed t) : xs) ((Bracket _ t') : ys) =
      if t == t'
        then go xs ys
        else Left x

part1 input =
  let rows = parse input
      corrupted = lefts $ map rowType rows
   in sum . map scoreIllegal $ corrupted

part2 input =
  let rows = parse input
      incomplete = rights $ map rowType rows
      middle = genericLength incomplete / 2
   in (sort . map scoreCompletion) incomplete !! floor middle

main :: IO ()
main = do
  example <- readFile "../input/day10.example"
  real <- readFile "../input/day10.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)