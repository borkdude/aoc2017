module Day01 where

import Data.Char (digitToInt, isSpace)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Util (liftPredMaybe)
import Debug.Trace

input :: IO [Int]
input = mapMaybe safeDigit <$> readFile "resources/day1.txt" where
  safeDigit = fmap digitToInt . liftPredMaybe (not . isSpace)

sumMatching (x:xs) = snd $ foldl' countSame (x,0) (xs ++ [x]) where
  countSame (a, sum) n
    | a == n = (n, sum + n)
    | otherwise = (n, sum)

part1 = sumMatching <$> input

sumMatching2 xs = foldl' countSame 0 xs' where
  xs' = zip xs (drop l $ cycle xs)
  l = length xs `div` 2
  countSame sum (a, b)
    | a == b = sum + b
    | otherwise = sum

part2 = sumMatching2 <$> input
