module Day02 where

-- apply a function to a thing inside a thing
-- e.g. ffmap (+1) [Just 1]
ffmap a b = (fmap . fmap) a b

input :: IO [[Int]]
input = ffmap read . map words . lines <$>
  readFile "resources/day2.txt"

-- it would be nicer to read in reverse: read the file, then split it
-- into lines, then make words from a sentence and then parse each
-- word to an Int
-- let's define some infix stuff

(|>) = flip (.) -- f |> g, first f, then g

infixr 6 |$>
(|$>) a b = fmap b a -- Just 1 |$> (+1) == Just 2

-- better?
input2 :: IO [[Int]]
input2 = readFile "resources/day2.txt" |$>
  lines |> map words |> ffmap read  

diff :: [Int] -> Int
diff = (-) <$> maximum <*> minimum

part1 :: IO Int
part1 = sum . map diff <$> input
