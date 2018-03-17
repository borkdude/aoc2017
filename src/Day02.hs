module Day02 where

-- apply a function to a thing inside a thing
-- e.g. ffmap (+1) [Just 1] == [Just 2]
ffmap :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b) 
ffmap = fmap . fmap

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

-- better? arguably...
input2 :: IO [[Int]]
input2 = readFile "resources/day2.txt" |$>
  lines |> map words |> ffmap read  

diff :: [Int] -> Int
diff = (-) <$> maximum <*> minimum

part1 :: IO Int
part1 = input2 |$> map diff |> sum

divisibles :: [Int] -> (Int, Int)
divisibles xs = head [ (x, y) | x <- xs, y <- xs,
                       x /= y, x `mod` y == 0]

part2 :: IO Int
part2 = input2 |$>
  map (divisibles |> uncurry div) |> sum 
