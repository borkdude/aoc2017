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
diff xs =
  let highest = maximum xs
      lowest  = minimum xs in
    highest - lowest

part1 :: IO Int
part1 = do
  inp <- input
  let diffs = map diff inp in
    return $ sum diffs

-- sumMatching :: [Int] -> Int
-- sumMatching (x:xs) = snd $ foldl' countSame (x,0) (xs ++ [x]) where
--   countSame (a, sum) n
--     | a == n = (n, sum + n)
--     | otherwise = (n, sum)

-- part1 :: IO Int
-- part1 = sumMatching <$> input

-- sumMatching2 :: [Int] -> Int
-- sumMatching2 xs = foldl' countSame 0 xs' where
--   xs' = zip xs (drop l $ cycle xs)
--   l = length xs `div` 2
--   countSame sum (a, b)
--     | a == b = sum + b
--     | otherwise = sum

-- part2 :: IO Int
-- part2 = sumMatching2 <$> input
