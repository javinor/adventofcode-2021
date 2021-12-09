module Main where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Map (Map, (!?))
import qualified Data.Map as M
import Data.Set (Set, union, (\\))
import qualified Data.Set as S

data Point = Point {row :: Int, col :: Int} deriving (Show, Eq, Ord)

parse :: String -> Map Point Integer
parse input =
  let xss :: [[Integer]]
      xss = map (map read . filter (/= "") . splitOn "") . lines $ input
      coords :: [(Point, Integer)]
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

findNeighbors :: Map Point Integer -> Point -> Set Point
findNeighbors coords Point {row = r, col = c} =
  let potentialNeighbors = S.fromList [Point (r + 1) c, Point (r - 1) c, Point r (c + 1), Point r (c - 1)]
   in M.keysSet coords `S.intersection` potentialNeighbors

findMinHeights :: Map Point Integer -> Map Point Integer
findMinHeights coords = M.filterWithKey isMinHeight coords
  where
    isMinHeight point val =
      let neighbors = S.toList $ findNeighbors coords point
       in and . fmap (maybe True (> val) . (coords !?)) $ neighbors

findConnectedPoints :: Map Point Integer -> Point -> Set Point
findConnectedPoints coords start = go start S.empty S.empty
  where
    go :: Point -> Set Point -> Set Point -> Set Point
    go curr next visited =
      let neighbors = findNeighbors coords curr \\ visited
          nextWithNeighbors = next `union` neighbors
          visited' = S.insert curr visited
       in if S.null nextWithNeighbors
            then visited'
            else
              let (curr', next') = S.deleteFindMin nextWithNeighbors
               in go curr' next' visited'

findBasins :: Map Point Integer -> [Set Point]
findBasins coords =
  let lowPoints = M.keys $ findMinHeights coords
      coordsUnder9 = M.filter (< 9) coords
   in fmap (findConnectedPoints coordsUnder9) lowPoints

part1 :: String -> Integer
part1 input =
  let coords = parse input
      minHeights = findMinHeights coords
   in sum . map ((+ 1) . snd) . M.toList $ minHeights

part2 :: String -> Int
part2 input =
  let coords = parse input
      basins = findBasins coords
   in product . take 3 . reverse . sort . map length $ basins

main :: IO ()
main = do
  example <- readFile "../input/day09.example"
  real <- readFile "../input/day09.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)