module Day02 where

import Data.Char (digitToInt, isSpace)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Util (liftPredMaybe)
import Debug.Trace

input :: IO [[Int]]
input = (fmap . fmap) read . map words . lines <$> readFile "resources/day2.txt"

(|>) = flip (.)
(|$>) a b = fmap b a

input2 :: IO [[Int]]
input2 = readFile "resources/day2.txt" |$> (lines |> map words |> (fmap . fmap) read)


diff :: [Int] -> Int
diff = (-) <$> maximum <*> minimum

part1 :: IO Int
part1 = sum . map diff <$> input
