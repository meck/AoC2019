module Day4 (day04a, day04b) where

import           Control.Applicative            ( liftA2 )
import           Util                           ( digits
                                                , group
                                                )

grpLenFilter :: (Int -> Bool) -> Int -> Bool
grpLenFilter f = any (f . length) . group . digits

isIncr :: Int -> Bool
isIncr = and . mapAdj (<=) . digits where mapAdj f xs = zipWith f xs (tail xs)

parse :: (Read a, Enum a) => String -> [a]
parse s = [read x .. read $ tail x'] where (x, x') = break (== '-') s

day04a :: String -> String
day04a =
    show . length . filter (liftA2 (&&) isIncr (grpLenFilter (>= 2))) . parse

day04b :: String -> String
day04b =
    show . length . filter (liftA2 (&&) isIncr (grpLenFilter (== 2))) . parse
