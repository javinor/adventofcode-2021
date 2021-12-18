{-# LANGUAGE TupleSections #-}

module Main where

import Data.Char (digitToInt, intToDigit)
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
import Numeric (readHex, readInt, showIntAtBase)

data OpType
  = Sum
  | Product
  | Min
  | Max
  | GreaterThan
  | LessThan
  | EqualsTo
  deriving (Show)

data Packet
  = Literal {version :: Integer, value :: Integer}
  | Operator {version :: Integer, op :: OpType, packets :: [Packet]}
  deriving (Show)

readBin = fst . head . readInt 2 (`elem` "01") digitToInt

hexToBin :: Char -> String
hexToBin c =
  case c of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"
    _ -> error $ "Illegal hex digit: " <> show c

parseOpType typeId =
  case typeId of
    "000" -> Sum
    "001" -> Product
    "010" -> Min
    "011" -> Max
    "101" -> GreaterThan
    "110" -> LessThan
    "111" -> EqualsTo
    _ -> error $ "illegal typeID: " <> typeId

parse input = input >>= hexToBin

parseLiteral version bits =
  let (lit, remainingBits) = collect "" bits
   in (Literal version (readBin lit), remainingBits)
  where
    collect acc ('0' : bits') = (acc ++ take 4 bits', drop 4 bits')
    collect acc ('1' : bits') = collect (acc ++ take 4 bits') (drop 4 bits')
    collect _ bits' = error $ "unexpected input : " <> bits'

parseOperator version op ('0' : bits) =
  let (encoded, bits') = splitAt 15 bits
      lengthInBits = fromIntegral $ readBin encoded
      (subPacketBits, bits'') = splitAt lengthInBits bits'
      subPackets = parsePackets subPacketBits
   in (Operator version op subPackets, bits'')
parseOperator version op ('1' : bits) =
  let (encoded, bits') = splitAt 11 bits
      nSubPackets = readBin encoded
      (subPackets, bits'') = parsePacketCount (fromIntegral nSubPackets) bits'
   in (Operator version op subPackets, bits'')
parseOperator _ _ input = error $ "unable to parse operator from input: " <> input

parsePacketCount :: Int -> String -> ([Packet], String)
parsePacketCount 0 bits = ([], bits)
parsePacketCount n bits =
  let (packet, bits') = parsePacket bits
      (packets, bits'') = parsePacketCount (n - 1) bits'
   in (packet : packets, bits'')

parsePackets :: String -> [Packet]
parsePackets bits = reverse $ go [] bits
  where
    go :: [Packet] -> String -> [Packet]
    go acc "" = acc
    go acc bits =
      let (packet, bits') = parsePacket bits
       in go (packet : acc) bits'

parsePacket bits =
  let (version, bits') = splitAt 3 bits
      (typeId, bits'') = splitAt 3 bits'
   in case typeId of
        "100" -> parseLiteral (readBin version) bits''
        _ -> parseOperator (readBin version) (parseOpType typeId) bits''

sumVersions :: Packet -> Integer
sumVersions (Literal v _) = v
sumVersions (Operator v _ packets) =
  v + sum (fmap sumVersions packets)

eval :: Packet -> Integer
eval (Literal _ val) = val
eval (Operator _ Sum packets) = sum $ fmap eval packets
eval (Operator _ Product packets) = product $ fmap eval packets
eval (Operator _ Min packets) = minimum $ fmap eval packets
eval (Operator _ Max packets) = maximum $ fmap eval packets
eval (Operator _ GreaterThan [left, right]) = if eval left > eval right then 1 else 0
eval (Operator _ LessThan [left, right]) = if eval left < eval right then 1 else 0
eval (Operator _ EqualsTo [left, right]) = if eval left == eval right then 1 else 0
eval p = error $ "Illegal packets: " <> show p

part1 input =
  let bits = parse input
      (packet, _) = parsePacket bits
   in sumVersions packet

part2 input =
  let bits = parse input
      (packet, _) = parsePacket bits
   in eval packet

main :: IO ()
main = do
  example <- readFile "../input/day16.example"
  let exampleA = "38006F45291200"
  let exampleB = "EE00D40C823060"
  let exampleC = "8A004A801A8002F478"
  let exampleD = "620080001611562C8802118E34"
  let exampleE = "C0015000016115A2E0802F182340"
  let exampleF = "A0016C880162017C3686B18A3D4780"

  let exampleA2 = "C200B40A82"
  let exampleB2 = "04005AC33890"
  let exampleC2 = "880086C3E88112"
  let exampleD2 = "CE00C43D881120"
  let exampleE2 = "D8005AC2A8F0"
  let exampleF2 = "F600BC2D8F"
  let exampleG2 = "9C005AC2F8F0"
  let exampleH2 = "9C0141080250320F1802104A08"

  real <- readFile "../input/day16.real"

  putStrLn $ "part1 example: " <> show (part1 example)
  putStrLn $ "part1 exampleA " <> show (part1 exampleA)
  putStrLn $ "part1 exampleB: " <> show (part1 exampleB)
  putStrLn $ "part1 exampleC: " <> show (part1 exampleC)
  putStrLn $ "part1 exampleD: " <> show (part1 exampleD)
  putStrLn $ "part1 exampleE: " <> show (part1 exampleE)
  putStrLn $ "part1 exampleF: " <> show (part1 exampleF)
  putStrLn $ "part1 real: " <> show (part1 real)

  putStrLn $ "part2 exampleA: " <> show (part2 exampleA2)
  putStrLn $ "part2 exampleB: " <> show (part2 exampleB2)
  putStrLn $ "part2 exampleC: " <> show (part2 exampleC2)
  putStrLn $ "part2 exampleD: " <> show (part2 exampleD2)
  putStrLn $ "part2 exampleE: " <> show (part2 exampleE2)
  putStrLn $ "part2 exampleF: " <> show (part2 exampleF2)
  putStrLn $ "part2 exampleG: " <> show (part2 exampleG2)
  putStrLn $ "part2 exampleH: " <> show (part2 exampleH2)

  putStrLn $ "part2 real: " <> show (part2 real)