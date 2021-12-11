{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.Bifunctor
import Data.Either (lefts, rights)
import qualified Data.Foldable as M
import Data.List (foldl', genericLength, intercalate, sort)
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, isJust)
import Data.Set (Set, (\\))
import qualified Data.Set as S

data Coord = Point {row :: Int, col :: Int} deriving (Show, Eq, Ord)

type Board = Map Coord Integer

parse :: String -> Board
parse input =
  let xss :: [[Integer]]
      xss = map (map read . filter (/= "") . splitOn "") . lines $ input
      coords :: [(Coord, Integer)]
      coords =
        concat $
          zipWith
            ( \rIndex row ->
                zipWith
                  (\cIndex col -> (Point rIndex cIndex, col))
                  [0 ..]
                  row
            )
            [0 ..]
            xss
   in M.fromList coords

getImmediateNeighbors :: Coord -> Set Coord
getImmediateNeighbors Point {row, col} =
  S.fromList
    [ Point (row + dr) (col + dc)
      | dr <- [-1, 0, 1],
        dc <- [-1, 0, 1],
        dr /= 0 || dc /= 0
    ]

inBoard :: Board -> Coord -> Bool
inBoard board = isJust . (`M.lookup` board)

getNeighbors :: Set Coord -> Board -> Set Coord
getNeighbors coords board =
  let neighbors = ((\\ coords) . S.unions . S.map getImmediateNeighbors) coords
   in S.filter (inBoard board) neighbors

incrementBoard :: Set Coord -> Board -> Board
incrementBoard flashingCoords board =
  let neighbors = getNeighbors flashingCoords board
      inc coord val
        | Just 0 == M.lookup coord board = 0
        | S.member coord flashingCoords = 0
        | S.member coord neighbors =
          let nFlashingNeighbors = toInteger . S.size $ flashingCoords `S.intersection` getImmediateNeighbors coord
           in val + nFlashingNeighbors
        | otherwise = val
   in M.mapWithKey inc board

flash :: Board -> (Integer, Board)
flash board =
  let flashingCoords = M.keysSet $ M.filter (> 9) board
      nFlashingCoords = toInteger $ S.size flashingCoords
   in if nFlashingCoords == 0
        then (0, board)
        else
          let boardAfterIncrement = incrementBoard flashingCoords board
              (n, board') = flash boardAfterIncrement
           in (n + nFlashingCoords, board')

step :: Board -> Int -> (Integer, Board)
step board 0 = (0, board)
step board n =
  let boardPlus1 = fmap (+ 1) board
      (nFlashes, boardAfterFlash) = flash boardPlus1
   in Data.Bifunctor.first (nFlashes +) $ step boardAfterFlash (n - 1)

showBoard board =
  intercalate ['\n'] $
    fmap
      ( \r ->
          concatMap
            (\c -> maybe "-" show (M.lookup (Point r c) board))
            [0 .. 9]
      )
      [0 .. 9]

part1 input =
  let board = parse input
      (n, board') = step board 100
   in n

part2 input = go (parse input) 1
  where
    go :: Board -> Integer -> Integer
    go board nStep =
      let (_, board') = step board 1
       in if M.all (== 0) board'
            then nStep
            else go board' (nStep + 1)

main :: IO ()
main = do
  example <- readFile "../input/day11.example"
  real <- readFile "../input/day11.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)