module Day4 (day04a, day04b) where

import           Control.Applicative            ( liftA2 )
import           Util                           ( group )

grpLenFilter :: Eq a => (Int -> Bool) -> [a] -> Bool
grpLenFilter f = any (f . length) . group

isIncr :: Ord a => [a] -> Bool
isIncr = and . mapAdj (<=) where mapAdj f xs = zipWith f xs (tail xs)

parse :: String -> [String]
parse s = show <$> [read x ::Int .. read $ tail x'] where (x, x') = break (== '-') s

day04a :: String -> String
day04a =
    show . length . filter (liftA2 (&&) isIncr (grpLenFilter (>= 2))) . parse

day04b :: String -> String
day04b =
    show . length . filter (liftA2 (&&) isIncr (grpLenFilter (== 2))) . parse
