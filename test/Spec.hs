module Main where

import Test.HUnit
import qualified Day01
import qualified Day02

testDay01 = TestCase (do x <- Day01.part1
                         assertEqual "Day 01 part 1" 995 x
                         y <- Day01.part2
                         assertEqual "Day 01 part 2" 1130 y)

testDay02 = TestCase (do x <- Day02.part1
                         assertEqual "Day 02 part 1" 44887 x
                         -- y <- Day01.part2
                         -- assertEqual "Day 01 part 2" 1130 y
                     )


tests :: Test
tests = TestList [TestLabel "Day01" testDay01
                 ,TestLabel "Day02" testDay02
                 ]

main :: IO Counts
main = runTestTT tests
