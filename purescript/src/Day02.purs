module Day02 where

import Prelude
import Data.List (List, foldl)
import Data.List as List
import Data.String (Pattern(..), split)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)
import Utils (unsafeParseInt10)

data Dir
  = Up
  | Down
  | Forward

data Command
  = Command Dir Int

type Position
  = { horizontal :: Int, depth :: Int }

type PositionWithAim
  = { horizontal :: Int, depth :: Int, aim :: Int }

parseCommand :: String -> Command
parseCommand str =
  unsafePartial
    $ let
        [ cmd, n ] = split (Pattern " ") str

        dir = case cmd of
          "forward" -> Forward
          "down" -> Down
          _ -> Up
      in
        Command dir (unsafeParseInt10 n)

parseInput :: String -> List Command
parseInput input =
  input
    # split (Pattern "\n")
    # map parseCommand
    # List.fromFoldable

part1 :: String -> Int
part1 input =
  let
    pos =
      input
        # parseInput
        # foldl move ({ horizontal: 0, depth: 0 })
  in
    pos.depth * pos.horizontal
  where
  move :: Position -> Command -> Position
  move pos (Command Up n) = pos { depth = pos.depth - n }

  move pos (Command Down n) = pos { depth = pos.depth + n }

  move pos (Command Forward n) = pos { horizontal = pos.horizontal + n }

part2 :: String -> Int
part2 input =
  let
    pos =
      input
        # parseInput
        # foldl move ({ horizontal: 0, depth: 0, aim: 0 })
  in
    pos.depth * pos.horizontal
  where
  move :: PositionWithAim -> Command -> PositionWithAim
  move pos (Command Up n) = pos { aim = pos.aim - n }
  move pos (Command Down n) = pos { aim = pos.aim + n }
  move pos (Command Forward n) =
    pos
      { horizontal = pos.horizontal + n
      , depth = pos.depth + pos.aim * n
      }

main :: Effect Unit
main =
  launchAff_ do
    example <- readTextFile UTF8 "../input/day02.example"
    real <- readTextFile UTF8 "../input/day02.real"
    log $ "part1 example: " <> (show $ part1 example)
    log $ "part1 real: " <> (show $ part1 real)
    log $ "part2 example: " <> (show $ part2 example)
    log $ "part2 real: " <> (show $ part2 real)
