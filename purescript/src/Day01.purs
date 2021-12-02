module Day01 where

import Prelude

import Data.Int (fromString)
import Data.List (List(..), fromFoldable, (:))
import Data.Maybe (fromJust)
import Data.String.Utils (lines)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Partial.Unsafe (unsafePartial)

parseInput :: String -> List Int
parseInput input =
  unsafePartial
    $ fromFoldable
    $ (lines input <#> fromString <#> fromJust)

countIncreases :: List Int -> Int
countIncreases = go 0
  where
  go :: Int -> List Int -> Int
  go acc (x : x' : xs) =
    let
      dx = if x' > x then 1 else 0
    in
      go (acc + dx) (x' : xs)

  go acc _ = acc

part1 :: String -> Int
part1 = countIncreases <<< parseInput

part2 :: String -> Int
part2 = countIncreases <<< sumTriplets <<< parseInput
  where
  sumTriplets :: List Int -> List Int
  sumTriplets (x : y : z : rest) = (x + y + z) : (sumTriplets $ y : z : rest)
  sumTriplets _ = Nil

main :: Effect Unit
main =
  launchAff_ do
    example <- readTextFile UTF8 "../input/day01.example"
    real <- readTextFile UTF8 "../input/day01.real"
    log $ "part1 example: " <> (show $ part1 example)
    log $ "part1 real: " <> (show $ part1 real)
    log $ "part2 example: " <> (show $ part2 example)
    log $ "part2 real: " <> (show $ part2 real)
