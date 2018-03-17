module Day02 where

import Data.Char (digitToInt, isSpace)
import Data.List (foldl')
import Data.Maybe (mapMaybe)
import Util (liftPredMaybe)
import Debug.Trace

input :: IO [[Int]]
input = do
  contents <- readFile "resources/day2.txt"
  let ls = (fmap . fmap) read $ map words $ lines contents
  return ls

diff :: [Int] -> Int
diff = (-) <$> maximum <*> minimum

part1 :: IO Int
part1 = sum . map diff <$> input
