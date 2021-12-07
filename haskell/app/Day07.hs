module Main where

import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = fmap read . splitOn ","

alignmentCost :: Num a => (Int -> a) -> Int -> [Int] -> a
alignmentCost fuelFn pos = sum . map (fuelFn . abs . (-) pos )

minFuelCost :: (Ord a, Num a) => (Int -> a) -> String -> a
minFuelCost fuelFn input =
  let xs = parse input
      minX = minimum xs
      maxX = maximum xs
   in minimum [alignmentCost fuelFn i xs | i <- [minX .. maxX]]

part1 = minFuelCost id

part2 = minFuelCost (arithSum . toRational)
  where
    arithSum n = n * (n + 1) / 2

main :: IO ()
main = do
  example <- readFile "../input/day07.example"
  real <- readFile "../input/day07.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)