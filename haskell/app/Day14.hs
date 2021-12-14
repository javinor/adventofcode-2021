{-# LANGUAGE TupleSections #-}

module Main where

import Data.Function (on)
import Data.List (foldl', foldr, group, intercalate, maximumBy, minimumBy, sort)
import qualified Data.List as L
import Data.List.Split (splitOn)
import Data.Map (Map)
import qualified Data.Map as M
import Debug.Trace (trace)

parseRule rule =
  let [from, to] = splitOn " -> " rule
   in (from, to)

parse input =
  let (template : "" : rules) = splitOn "\n" input
      rules' = M.fromList $ fmap parseRule rules
   in (template, rules')

tick :: String -> Map String String -> String
tick template rules =
  let pairs = zipWith (\c1 c2 -> [c1, c2]) template (tail template)
      Just newElements = concat <$> traverse (`M.lookup` rules) pairs
   in head template : concat (zipWith (\c1 c2 -> [c1, c2]) newElements (tail template))

tick2 :: Map String Int -> Map String String -> Map String Int
tick2 template rules =
  M.foldlWithKey
    ( \acc pair n ->
        let [el1, el2] = pair
            Just newEl = head <$> M.lookup pair rules
            p1 = [el1, newEl]
            p2 = [newEl, el2]
         in M.insertWith (+) p1 n $ M.insertWith (+) p2 n acc
    )
    M.empty
    template

part1 input =
  let (template, rules) = parse input
      template' = foldl' (\t _ -> tick t rules) template [1 .. 10]
      groups = fmap (\x -> ([head x], length x)) . group . sort $ template'
      minEl = minimumBy (compare `on` snd) groups
      maxEl = maximumBy (compare `on` snd) groups
   in (minEl, maxEl, snd maxEl - snd minEl)

part2 input =
  let (template, rules) = parse input
      templateMap = M.fromListWith (+) . fmap (,1) $ zipWith (\c1 c2 -> [c1, c2]) template (tail template)
      templateMap' = foldl' (\t _ -> tick2 t rules) templateMap [1 .. 40]
      countMap = M.foldlWithKey (\acc [_, c] n -> M.insertWith (+) [c] n acc) M.empty templateMap'
      countMap' = M.insertWith (+) [head template] 1 countMap

      minEl = minimumBy (compare `on` snd) $ M.toList countMap'
      maxEl = maximumBy (compare `on` snd) $ M.toList countMap'
   in (minEl, maxEl, snd maxEl - snd minEl)

main :: IO ()
main = do
  example <- readFile "../input/day14.example"
  real <- readFile "../input/day14.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 real: " <> show (part1 real)
  putStrLn $ "part2 example: " <> show (part2 example)
  putStrLn $ "part2 real: " <> show (part2 real)