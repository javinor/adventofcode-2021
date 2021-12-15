{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (intToDigit)
import Data.Function (on)
import Data.List (foldl', foldr, group, intercalate, maximumBy, minimumBy, sort)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust, fromMaybe)
import Data.Set (Set, (\\))
import qualified Data.Set as S
import Debug.Trace (trace)

data Coord = Coord {row :: Int, col :: Int} deriving (Show, Eq, Ord)

type Cave = Map Coord Integer

parse :: String -> (Cave, Int, Int)
parse = parseIntegersToCave . parseStringToIntegers

parse2 :: String -> (Cave, Int, Int)
parse2 = parseIntegersToExtendedCave . parseStringToIntegers

parseStringToIntegers :: String -> [[Integer]]
parseStringToIntegers input = fmap (read . (: [])) <$> lines input

parseIntegersToCave :: [[Integer]] -> (Cave, Int, Int)
parseIntegersToCave xss =
  let nRows = length xss
      nCols = length (head xss)

      coords :: [(Coord, Integer)]
      coords =
        concat $
          zipWith
            ( \rIndex row ->
                zipWith
                  (\cIndex col -> (Coord rIndex cIndex, col))
                  [0 ..]
                  row
            )
            [0 ..]
            xss
   in (M.fromList coords, nRows, nCols)

incrementCave :: Integer -> [[Integer]] -> [[Integer]]
incrementCave k = fmap (fmap inc)
  where
    inc n =
      if n + k >= 10
        then (n - 1 + k) `mod` 9 + 1
        else n + k

parseIntegersToExtendedCave :: [[Integer]] -> (Cave, Int, Int)
parseIntegersToExtendedCave xss =
  ( M.fromList
      [ (Coord row col, calcRiskAt row col)
        | row <- [0 .. nExtRows - 1],
          col <- [0 .. nExtCols - 1]
      ],
    nExtRows,
    nExtCols
  )
  where
    nRows = length xss
    nExtRows = 5 * nRows
    nCols = length (head xss)
    nExtCols = 5 * nCols
    caves = [incrementCave (toInteger i) xss | i <- [0 .. (nRows - 1) + (nCols - 1)]]

    calcRiskAt row col =
      let (rowInc, row') = row `divMod` nRows
          (colInc, col') = col `divMod` nCols
       in caves !! (rowInc + colInc) !! row' !! col'

getNeighbors :: Coord -> Set Coord -> Int -> Int -> [Coord]
getNeighbors (Coord row col) visited nRows nCols =
  let neighbors =
        S.fromList
          [ Coord r c
            | (r, c) <- [(row + 1, col), (row - 1, col), (row, col + 1), (row, col - 1)],
              r < nRows && c < nCols && r >= 0 && c >= 0
          ]
   in S.toList $ neighbors \\ visited

dijkstra :: Map Coord Integer -> Int -> Int -> Integer
dijkstra cave nRows nCols = go visited distances distSet
  where
    visited = S.empty
    start = Coord 0 0
    end = Coord (nRows - 1) (nCols - 1)
    distances = M.singleton start 0
    distSet = S.singleton (0, start)

    updateDistances :: Integer -> Coord -> (Map Coord Integer, Set (Integer, Coord)) -> (Map Coord Integer, Set (Integer, Coord))
    updateDistances acc neigh (distances, distSet) =
      let distSet' = case M.lookup neigh distances of
            Nothing -> distSet
            Just d -> S.delete (d, neigh) distSet
          distances' = M.insertWith min neigh (acc + fromJust (M.lookup neigh cave)) distances
          distSet'' = S.insert (fromJust (M.lookup neigh distances'), neigh) distSet'
       in (distances', distSet'')

    go :: Set Coord -> Map Coord Integer -> Set (Integer, Coord) -> Integer
    go visited distances distSet =
      let ((dist, coord), distSet') = fromJust $ S.minView distSet
       in if coord == end
            then dist
            else
              let visited' = S.insert coord visited
                  neighbors = getNeighbors coord visited' nRows nCols
                  (distances', distSet'') = foldr (updateDistances dist) (distances, distSet') neighbors
               in go visited' distances' distSet''

part1 :: [Char] -> Integer
part1 input =
  let (cave, nRows, nCols) = parse input
   in dijkstra cave nRows nCols

part2 input =
  let (cave, nRows, nCols) = parse2 input
   in dijkstra cave nRows nCols

main :: IO ()
main = do
  example <- readFile "../input/day15.example"
  real <- readFile "../input/day15.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)