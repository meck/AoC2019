module Day5 (day05a, day05b) where

import           Data.List.Split                ( splitOn )
import           CGC

day05a :: String -> String
day05a = show . last . flip evalCGCprg [1] . fmap read . splitOn ","

day05b :: String -> String
day05b = show . last . flip evalCGCprg [5] . fmap read . splitOn ","
