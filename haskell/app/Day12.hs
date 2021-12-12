module Main where

import Data.Char (isLower, isUpper)
import Data.List (intercalate)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromJust)
import Data.Semigroup (All (All, getAll))
import Data.Set (Set)
import qualified Data.Set as S
import Debug.Trace (trace)

type Graph = Map String [String]

type Node = String

type Path = [String]

data Visited
  = CannotVisitAgain (Set Node)
  | CanVisitAgain (Set Node)
  deriving (Show)

parse :: String -> Graph
parse = M.unionsWith (++) . fmap parseRow . lines
  where
    parseRow str =
      let [from, to] = splitOn "-" str
       in M.fromList [(from, [to]), (to, [from])]

isRestrictedNode :: String -> Bool
isRestrictedNode node = node /= "start" && node /= "end" && all isLower node

canVisit :: Visited -> Node -> Bool
canVisit visited node
  | node == "end" = True
  | all isUpper node = True
  | isRestrictedNode node =
    case visited of
      CannotVisitAgain vs -> not $ S.member node vs
      CanVisitAgain _ -> True
  | otherwise = False

nextVisited :: Node -> Visited -> Visited
nextVisited node visited
  | isRestrictedNode node =
    case visited of
      CannotVisitAgain vs -> CannotVisitAgain $ S.insert node vs
      CanVisitAgain vs ->
        if S.member node vs
          then CannotVisitAgain vs
          else CanVisitAgain (S.insert node vs)
  | otherwise = visited

dfs :: Graph -> [Node] -> Node -> Visited -> [Path]
dfs _ path "end" _ = ["end" : path]
dfs g path curr visited =
    let path' = curr : path
        next = filter (canVisit visited) $ fromJust $ M.lookup curr g
     in concatMap (\n -> dfs g path' n (nextVisited n visited)) next

part1 input =
  let g = parse input
   in length $ dfs g [] "start" (CannotVisitAgain S.empty)

part2 input =
  let g = parse input
   in length $ dfs g [] "start" (CanVisitAgain S.empty)

main :: IO ()
main = do
  example <- readFile "../input/day12.example"
  larger <- readFile "../input/day12.larger.example"
  largest <- readFile "../input/day12.largest.example"
  real <- readFile "../input/day12.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 larger: " <> show (part1 larger)
  putStrLn $ "part1 largest: " <> show (part1 largest)
  putStrLn $ "part1 real: " <> show (part1 real)

  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 larger: " <> show (part2 larger)
  putStrLn $ "part2 largest: " <> show (part2 largest)
  putStrLn $ "part2 real: " <> show (part2 real) -- 148962