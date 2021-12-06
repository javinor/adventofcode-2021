{-# LANGUAGE TupleSections #-}

module Main where

import Data.IntMap (IntMap, foldl', fromListWith, toList)
import Data.List (replicate)
import Data.List.Split (splitOn)

parse :: String -> [Int]
parse = fmap read . splitOn ","

tick :: [Int] -> [Int]
tick timers =
  let recurringTimerValue = 6
      newTimerValue = 8
      nNewTimers = length $ filter (== 0) timers
      updateTimer t = if t == 0 then recurringTimerValue else t - 1
      timers' = fmap updateTimer timers
   in timers' ++ replicate nNewTimers newTimerValue

tick' :: IntMap Integer -> IntMap Integer
tick' timers =
  fromListWith (+) $ do
    (key, value) <- toList timers
    if key == 0 then [(6, value), (8, value)] else [(key - 1, value)]

part1 input =
  let initialTimers = parse input
   in length $ iterate tick initialTimers !! 80

part2 input =
  let initialTimers = fromListWith (+) . map (,1) . parse $ input
      gen256 = iterate tick' initialTimers !! 256
   in foldl' (+) 0 gen256

main :: IO ()
main = do
  example <- readFile "../input/day06.example"
  real <- readFile "../input/day06.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)