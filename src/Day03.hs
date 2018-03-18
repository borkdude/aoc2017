module Day03 where

import Util (debug)
{-

-- NB: every bottom right tile is an odd square: 1^2, 3^3, 5^2, etc.
-- Places: (0,0), (1,1), (2,2), so (odd - 1 / 2, odd - 1 / 2)
-- So first find the closest odd square

x1 = sqrt 289326 -- 537.8903233931617
x2 = 537 ^ 2 -- 288369, so at position (537 - 1 / 2, 537 - 1 / 2) == (268,268)
-- diff = input - x2 -- 957
--    957                  956                   418             
-- (268,268) --1 right-> (269,268) --538 up-> (269,-270) --418 left-> (-149,-270)
-- so the distance is == 419

input = 289326 :: Int

solve :: Int -> Int
solve n = let
  -- TODO: fix 4
  squareRoot = floor (sqrt (fromIntegral n :: Float))
  oddSqrt = if odd squareRoot then squareRoot else squareRoot - 1
  posX = (oddSqrt - 1) `div` 2
  posY = posX
  diff = n - (oddSqrt ^ 2)
  -- move one to the right if possible
  right = min diff 1 `debug` show [("squareRoot", squareRoot),
                                   ("oddSqrt", oddSqrt),
                                   ("diff", diff),
                                   ("posX", posX),
                                   ("posY", posY)]
  diff' = diff - right `debug` show [("right", right)]
  posX' = posX + right `debug` show [("diff'", diff')]
  -- move up oddSqrt + 1 or with whatever is left
  up = min diff' (oddSqrt + 1)
  diff'' = diff' - up `debug` (show "up: " ++ show up)
  posY' = posY - up
  -- move left with whatever is left
  posX'' = posX' - diff'' `debug` show [("posY'", posY')]
  in abs posX'' + abs posY' `debug` show [("posX''",posX''), ("posY''",posY')]

part1 :: Int
part1 = solve input
-}
