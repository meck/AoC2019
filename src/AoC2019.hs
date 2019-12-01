module AoC2019 (inputFp, argLookup) where

import           Day1
import           Day2
import           Day3
import           Day4
import           Day5
import           Day6
import           Day7
import           Day8
import           Day9
import           Day10
import           Day11
import           Day12
import           Day13
import           Day14
import           Day15
import           Day16
import           Day17
import           Day18
import           Day19
import           Day20
import           Day21
import           Day22
import           Day23
import           Day24
import           Day25

inputFp :: String -> FilePath
inputFp n = "input-data/" ++ n ++ ".txt"

argLookup :: [(String, String -> String)]
argLookup =
    [ ("1a", day01a)
    , ("1b", day01b)
    , ("2a", day02a)
    , ("2b", day02b)
    , ("3a", day03a)
    , ("3b", day03b)
    , ("4a", day04a)
    , ("4b", day04b)
    , ("5a", day05a)
    , ("5b", day05b)
    , ("6a", day06a)
    , ("6b", day06b)
    , ("7a", day07a)
    , ("7b", day07b)
    , ("8a", day08a)
    , ("8b", day08b)
    , ("9a", day09a)
    , ("9b", day09b)
    , ("10a", day10a)
    , ("10b", day10b)
    , ("11a", day11a)
    , ("11b", day11b)
    , ("12a", day12a)
    , ("12b", day12b)
    , ("13a", day13a)
    , ("13b", day13b)
    , ("14a", day14a)
    , ("14b", day14b)
    , ("15a", day15a)
    , ("15b", day15b)
    , ("16a", day16a)
    , ("16b", day16b)
    , ("17a", day17a)
    , ("17b", day17b)
    , ("18a", day18a)
    , ("18b", day18b)
    , ("19a", day19a)
    , ("19b", day19b)
    , ("20a", day20a)
    , ("20b", day20b)
    , ("21a", day21a)
    , ("21b", day21b)
    , ("22a", day22a)
    , ("22b", day22b)
    , ("23a", day23a)
    , ("23b", day23b)
    , ("24a", day24a)
    , ("24b", day24b)
    , ("25a", day25a)
    , ("25b", day25b)
    ]
