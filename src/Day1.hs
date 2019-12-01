module Day1 (day01a, day01b) where

fuel :: Int -> Int
fuel x = x `div` 3 - 2

fuel' :: Int -> [Int]
fuel' = takeWhile (> 0) . iterate fuel . fuel

day01a :: String -> String
day01a = show . sum . fmap (fuel . read) . lines

day01b :: String -> String
day01b = show . sum . (fuel' . read =<<) . lines
