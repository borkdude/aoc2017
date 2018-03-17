module Main where

import Test.HUnit
import qualified Day01

testDay01 = TestCase (do x <- Day01.part1
                         assertEqual "Day 01 part 1" 995 x
                         y <- Day01.part2
                         assertEqual "Day 01 part 2" 1130 y)
tests :: Test
tests = TestList [TestLabel "Day01" testDay01
                  -- ,TestLabel "Day02" testDay02
                  ]

main :: IO Counts
main = runTestTT tests
