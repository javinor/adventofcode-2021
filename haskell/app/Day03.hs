module Main where

import Data.Char (digitToInt)
import Data.List (foldl', foldl1')

toDecimal :: [Int] -> Int
toDecimal = foldl' (\acc x -> acc * 2 + x) 0

data BitCriteria = MostCommon | LeastCommon

runBitCriteria criteria nums = go 0 nums
  where
    go :: Int -> [[Int]] -> [Int]
    go i [] = error "filtered out all numbers :'-("
    go _ [num] = num
    go i nums =
      let median = length nums `div` 2
          countOnesAtIndex = sum $ map (!! i) nums
          common = if 2 * countOnesAtIndex >= length nums then 1 else 0
          keep = case criteria of
            MostCommon -> common
            LeastCommon -> 1 - common
          nums' = filter ((== keep) . (!! i)) nums
       in go (i + 1) nums'

parse :: String -> [[Int]]
parse = map (map digitToInt) . lines

part1 input =
  let nums = parse input
      median = length nums `div` 2
      countOnes = foldl1' (zipWith (+)) nums
      gamma = map (\x -> if x > median then 1 else 0) countOnes
      epsilon = map (1 -) gamma
   in toDecimal gamma * toDecimal epsilon

part2 input =
  let nums = parse input
      oxygenGenerator = runBitCriteria MostCommon nums
      co2Scrubber = runBitCriteria LeastCommon nums
   in 
      toDecimal oxygenGenerator * toDecimal co2Scrubber

main :: IO ()
main = do
  example <- readFile "../input/day03.example"
  real <- readFile "../input/day03.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)