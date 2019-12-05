module Day5 (day05a, day05b) where

import           Data.List.Split                ( splitOn )
import           CGC

day05a :: String -> String
day05a = show . head . runCGC . flip initCGC [1] . fmap read . splitOn ","

day05b :: String -> String
day05b = show . head . runCGC . flip initCGC [5] . fmap read . splitOn ","
