module Main where

import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.Typeable (cast)
import GHC.IO (unsafePerformIO)

data Direction = Up | Down | Forward deriving (Show, Eq)

type Command = (Direction, Int)

data Position = Position {horizontal :: Int, depth :: Int} deriving (Show, Eq)

data PositionWithAim = PositionWithAim
  { hor :: Int, -- horizontal
    dep :: Int, -- depth
    aim :: Int
  }
  deriving (Show, Eq)

mkDir :: String -> Maybe Direction
mkDir str =
  case str of
    "up" -> Just Up
    "down" -> Just Down
    "forward" -> Just Forward
    _ -> Nothing

command :: Direction -> Int -> Command
command = (,)

parseRow :: String -> Maybe Command
parseRow row =
  case words row of
    [dir, n] -> command <$> mkDir dir <*> Just (read n)
    _ -> Nothing

parse :: String -> Maybe [Command]
parse = traverse parseRow . lines

part1 input =
  let cmds = fromMaybe [] (parse input)
      initialPosition = Position 0 0

      move :: Position -> Command -> Position
      move pos cmd =
        case cmd of
          (Up, n) -> pos {depth = depth pos - n}
          (Down, n) -> pos {depth = depth pos + n}
          (Forward, n) -> pos {horizontal = horizontal pos + n}

      finalPosition = foldl' move initialPosition cmds
   in depth finalPosition * horizontal finalPosition

part2 input =
  let cmds = fromMaybe [] (parse input)
      initialPosition = PositionWithAim 0 0 0

      move :: PositionWithAim -> Command -> PositionWithAim
      move pos cmd =
        case cmd of
          (Up, n) -> pos {aim = aim pos - n}
          (Down, n) -> pos {aim = aim pos + n}
          (Forward, n) -> pos {hor = hor pos + n, dep = dep pos + aim pos * n}

      finalPosition = foldl' move initialPosition cmds
   in --  finalPosition
      dep finalPosition * hor finalPosition

main :: IO ()
main = do
  example <- readFile "../input/day02.example"
  real <- readFile "../input/day02.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)