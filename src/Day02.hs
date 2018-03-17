module Day02 where

input :: IO [[Int]]
input = (fmap . fmap) read . map words . lines <$>
  readFile "resources/day2.txt"

-- it would be nicer to read in reverse: read the file, then split it
-- into lines, then make words from a sentence and then parse each
-- word to an Int
-- let's define some infix stuff

(.:) a = fmap . fmap $ a -- (.) . (.), three dots you know

(|>) = flip (.) -- f |> g, first f, then g

infixr 6 |$>
(|$>) a b = fmap b a -- Just 1 |$> (+1) == Just 2

-- better?
input2 :: IO [[Int]]
input2 = readFile "resources/day2.txt" |$>
  lines |> map words |> (read .:) 

diff :: [Int] -> Int
diff = (-) <$> maximum <*> minimum

part1 :: IO Int
part1 = sum . map diff <$> input
