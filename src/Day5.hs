module Day5 (day05a, day05b) where

import           Data.List.Split                ( splitOn )
import           CGC

day05a :: String -> String
day05a = show . head . flip evalCGC [1] . initCGC . fmap read . splitOn ","

day05b :: String -> String
day05b = show . head . flip evalCGC [5] . initCGC . fmap read . splitOn ","
