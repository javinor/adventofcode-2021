module Main where

import Data.Char (isLower, isUpper)
import Data.List (intercalate)
import qualified Data.List as L
import Data.List.Split (chunksOf, splitEvery, splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Semigroup (All (All, getAll))
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace)

data Fold = FoldX Int | FoldY Int deriving (Show)

parseCoord :: String -> (Int, Int)
parseCoord str =
  let [x, y] = fmap read . splitOn "," $ str
   in (x, y)

parseFold :: [Char] -> Fold
parseFold str =
  case str of
    'x' : nStr -> FoldX $ read nStr
    'y' : nStr -> FoldY $ read nStr
    _ -> error $ "invalid fold instruction: " <> str

parse :: String -> ([(Int, Int)], [Fold])
parse str =
  let [coordsStr, foldsStr] = splitOn "\n\n" str
      coords = parseCoord <$> lines coordsStr
      folds = parseFold . concat . splitOn "=" . drop 11 <$> lines foldsStr
   in (coords, folds)

foldPaper coords [] = coords
foldPaper coords (FoldX n : fs) =
  let coords' = map (\(x, y) -> if x < n then (x, y) else (n - (x - n), y)) coords
   in foldPaper coords' fs
foldPaper coords (FoldY n : fs) =
  let coords' = map (\(x, y) -> if y < n then (x, y) else (x, n - (y - n))) coords
   in foldPaper coords' fs

part1 input =
  let (coords, folds) = parse input
   in length $ L.nub $ foldPaper coords (take 1 folds)

part2 input =
  let (coords, folds) = parse input
      coordsSet = S.fromList $ foldPaper coords folds
      maxX = maximum $ fst <$> S.toList coordsSet
      maxY = maximum $ snd <$> S.toList coordsSet
   in intercalate "\n" $
        chunksOf (maxX + 1) $
          concat
            [ if S.member (x, y) coordsSet then "#" else "."
              | y <- [0 .. maxY],
                x <- [0 .. maxX]
            ]

main :: IO ()
main = do
  example <- readFile "../input/day13.example"
  real <- readFile "../input/day13.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example:\n\n" <> part2 example
  putStrLn $ "part2 real:\n\n" <> part2 real