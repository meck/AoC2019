module Day9 (day09a, day09b) where

import           Data.List.Split                ( splitOn )
import           CGC

day09a :: String -> String
day09a = show . head . flip evalCGC [1] . initCGC . fmap read . splitOn ","

day09b :: String -> String
day09b = show . head . flip evalCGC [2] . initCGC . fmap read . splitOn ","
