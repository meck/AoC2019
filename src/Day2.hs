module Day2 (day02a, day02b) where

import           Data.List.Split                ( splitOn )
import           CGC

solveB :: Int -> [Int] -> Int
solveB out m =
    go
        $   head
        $   dropWhile ((/= out) . snd)
        $   fmap (runCGCwithVN m)
        <$> zip allNounVerb allNounVerb
  where
    allNounVerb = [ (a, b) | a <- [0 .. 99], b <- [0 .. 99] ]
    go ((n, v), _) = 100 * n + v

runCGCwithVN :: [Int] -> (Int, Int) -> Int
runCGCwithVN m vn =
    head $ readMem $ execCGC $ setVerbAndNoun vn $ initCGC m

day02a :: String -> String
day02a = show . flip runCGCwithVN (12, 2) . fmap read . splitOn ","

day02b :: String -> String
day02b = show . solveB 19690720 . fmap read . splitOn ","
