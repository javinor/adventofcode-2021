module Main where

import Data.List (concatMap, elemIndex, find, findIndex, foldl', intersect, nub, sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust)
import Data.Traversable (traverse)

parse :: String -> [([String], [String])]
parse input =
  let rows = map (splitOn " | ") $ lines input
   in map
        ( \[left, right] ->
            ( map sort $ words left,
              map sort $ words right
            )
        )
        rows

asNumber :: [Int] -> Integer
asNumber = toInteger . foldl' ((+) . (* 10)) 0

intersectionHasLength :: String -> Int -> String -> Bool
intersectionHasLength xs n = (== n) . length . intersect xs

calcOutputValue :: [String] -> [String] -> Integer
calcOutputValue patterns outputDigits =
  let one = fromJust $ find ((== 2) . length) patterns
      four = fromJust $ find ((== 4) . length) patterns
      seven = fromJust $ find ((== 3) . length) patterns
      eight = fromJust $ find ((== 7) . length) patterns
      nine = head $ filter (intersectionHasLength four 4) $ filter ((== 6) . length) patterns
      zero = head $ filter (/= nine) $ filter (intersectionHasLength seven 3) $ filter ((== 6) . length) patterns
      six = head $ filter (/= nine) $ filter (/= zero) $ filter ((== 6) . length) patterns
      three = head $ filter (intersectionHasLength seven 3) $ filter ((== 5) . length) patterns
      five = head $ filter (intersectionHasLength six 5) $ filter ((== 5) . length) patterns
      two = head $ filter (/= five) $ filter (/= three) $ filter ((== 5) . length) patterns

      digits = [zero, one, two, three, four, five, six, seven, eight, nine]
   in asNumber . fromJust . traverse (`elemIndex` digits) $ outputDigits

part1 input =
  length $
    filter (`elem` [2, 3, 4, 7])
      . map length
      . concatMap snd
      . parse
      $ input

part2 = sum . map (uncurry calcOutputValue) . parse

main :: IO ()
main = do
  example <- readFile "../input/day08.example"
  example2 <- readFile "../input/day08.larger.example"
  real <- readFile "../input/day08.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 example2: " <> show (part1 example2)
  putStrLn $ "part1 real: " <> show (part1 real)

  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 example2: " <> show (part2 example2)
  putStrLn $ "part2 real: " <> show (part2 real)