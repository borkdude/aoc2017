module Main where

import Test.HUnit
import qualified Day01
import qualified Day02
import qualified Day03

testDay01 = TestCase (do p1 <- Day01.part1
                         assertEqual "Day 01 part 1" 995 p1
                         p2 <- Day01.part2
                         assertEqual "Day 01 part 2" 1130 p2)

testDay02 = TestCase (do p1 <- Day02.part1
                         assertEqual "Day 02 part 1" 44887 p1
                         p2 <- Day02.part2
                         assertEqual "Day 02 part 2" 242 p2)

testDay03 = TestCase $ assertEqual "Day 03 part 1" 419 Day03.part1

tests :: Test
tests = TestList [TestLabel "Day01" testDay01
                 ,TestLabel "Day02" testDay02
                 ,TestLabel "Day03" testDay03]

main :: IO Counts
main = runTestTT tests
