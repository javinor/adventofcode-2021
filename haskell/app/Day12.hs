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

type Graph = Map Cave [Cave]

type Path = [Cave]

data Cave = Start | End | Small String | Big String deriving (Show, Eq, Ord)

data Visited
  = CannotVisitAgain (Set Cave)
  | CanVisitAgain (Set Cave)
  deriving (Show)

parseCave :: String -> Cave
parseCave "start" = Start
parseCave "end" = End
parseCave name
  | all isLower name = Small name
  | all isUpper name = Big name
  | otherwise = error $ "Failed to parse cave: " <> name

parse :: String -> Graph
parse = M.unionsWith (++) . fmap parseRow . lines
  where
    parseRow str =
      let [from, to] = parseCave <$> splitOn "-" str
       in M.fromList [(from, [to]), (to, [from])]

canVisit :: Visited -> Cave -> Bool
canVisit _ Start = False
canVisit _ End = True
canVisit _ (Big _) = True
canVisit visited (Small s) =
  case visited of
    CanVisitAgain _ -> True
    CannotVisitAgain vs -> not $ S.member (Small s) vs

nextVisited :: Cave -> Visited -> Visited
nextVisited End _ = CannotVisitAgain S.empty
nextVisited Start visited = visited
nextVisited (Big _) visited = visited
nextVisited (Small s) visited =
  case visited of
    CannotVisitAgain vs -> CannotVisitAgain $ S.insert (Small s) vs
    CanVisitAgain vs ->
      if S.member (Small s) vs
        then CannotVisitAgain vs
        else CanVisitAgain (S.insert (Small s) vs)

dfs :: Graph -> [Cave] -> Cave -> Visited -> [Path]
dfs _ path End _ = [End : path]
dfs g path curr visited =
  let path' = curr : path
      next = filter (canVisit visited) $ fromJust $ M.lookup curr g
   in concatMap (\n -> dfs g path' n (nextVisited n visited)) next

part1 input =
  let g = parse input
   in length $ dfs g [] Start (CannotVisitAgain S.empty)

part2 input =
  let g = parse input
   in length $ dfs g [] Start (CanVisitAgain S.empty)

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