module Main where

import Data.List (concatMap, group, reverse, sort)
import Data.List.Split (splitOn)
import Data.Map (fromListWith)

parseRow :: [Char] -> [Int]
parseRow = map read . concatMap (splitOn ",") . splitOn " -> "

fromTo :: (Ord a, Enum a) => a -> a -> [a]
fromTo from to = if from < to then [from .. to] else reverse [to .. from]

toLines :: [Int] -> [(Int, Int)]
toLines coords =
  let [x1, y1, x2, y2] = coords
   in if x1 == x2
        then [(x1, y) | y <- fromTo y1 y2]
        else
          if y1 == y2
            then [(x, y1) | x <- fromTo y1 y2]
            else []

toLines2 :: [Int] -> [(Int, Int)]
toLines2 coords =
  let [x1, y1, x2, y2] = coords
      xs = fromTo x1 x2
      ys = fromTo y1 y2
   in if x1 == x2
        then [(x1, y) | y <- ys]
        else
          if y1 == y2
            then [(x, y1) | x <- xs]
            else zip xs ys

parse input =
  concatMap (toLines . parseRow) (lines input)

parse2 input =
  concatMap (toLines2 . parseRow) (lines input)

part1 input =
  length $ filter (\g -> length g >= 2) . group . sort . parse $ input

part2 input =
  length $ filter (\g -> length g >= 2) . group . sort . parse2 $ input

main :: IO ()
main = do
  example <- readFile "../input/day05.example"
  real <- readFile "../input/day05.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)