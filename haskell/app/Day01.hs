module Main where


parse :: String -> [Int]
parse = map read . lines

consecutivePairs :: [a] -> [(a, a)]
consecutivePairs xs = zip xs (tail xs)

countIncreases :: Ord a => [a] -> Int
countIncreases = length . filter (uncurry (<)) . consecutivePairs

slidingWindowSum3 :: Num a => [a] -> [a]
slidingWindowSum3 xs =
  let ys = tail xs
      zs = tail ys
   in zipWith3 (\x y z -> x + y + z) xs ys zs

part1 :: String -> Int
part1 = countIncreases . parse

part2 :: String -> Int
part2 = countIncreases . slidingWindowSum3 . parse

main :: IO ()
main = do
  example <- readFile "../input/day01.example"
  real <- readFile "../input/day01.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)