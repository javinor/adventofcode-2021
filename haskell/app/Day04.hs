module Main where

import Data.List (concatMap, find, transpose)
import Data.List.Split (split, splitOn)

type Card = [[(Int, Bool)]]

parseNumbersToDraw :: String -> [Int]
parseNumbersToDraw = map read . splitOn ","

parseCardRow :: String -> [(Int, Bool)]
parseCardRow row =
  let row' = filter (/= "") $ splitOn " " row
   in map ((\n -> (n, False)) . read) row'

parseBingoCards :: [String] -> [Card]
parseBingoCards = map (map parseCardRow . splitOn "\n")

parse :: [Char] -> ([Int], [Card])
parse input =
  let (nums : cards) = splitOn "\n\n" input
   in (parseNumbersToDraw nums, parseBingoCards cards)

updateCards :: Int -> [Card] -> [Card]
updateCards num =
  fmap
    ( fmap
        ( map (\(n, b) -> if n == num then (n, True) else (n, b))
        )
    )

hasWinningRow :: [[(Int, Bool)]] -> Bool
hasWinningRow = any (and . fmap snd)

didCardWin :: [[(Int, Bool)]] -> Bool
didCardWin card = hasWinningRow card || hasWinningRow (transpose card)

findWinner :: [Card] -> Maybe Card
findWinner = find didCardWin

score :: Int -> Card -> Int
score num card =
  let unmarkedCells = fmap fst . concatMap (filter (not . snd)) $ card
   in num * sum unmarkedCells

playBingo [] _ = error "ran out of numbers to draw!!"
playBingo (x : xs) cards =
  let cards' = updateCards x cards
      winner = findWinner cards'
   in case winner of
        Nothing -> playBingo xs cards'
        Just w -> score x w

part1 input =
  let (numsToDraw, cards) = parse input
   in playBingo numsToDraw cards

playBingo2 [] _ = error "ran out of numbers to draw!!"
playBingo2 (x : xs) cards =
  let cards' = filter (not . didCardWin) . updateCards x $ cards
   in case cards' of
        [] -> error "all cards alread won!?"
        [c] -> playBingo xs [c]
        _ -> playBingo2 xs cards'

part2 input =
  let (numsToDraw, cards) = parse input
   in playBingo2 numsToDraw cards

main :: IO ()
main = do
  example <- readFile "../input/day04.example"
  real <- readFile "../input/day04.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)