module Day9 (day09a, day09b) where

import           Data.List.Split                ( splitOn )
import           CGC

day09a :: String -> String
day09a = show . last . flip evalCGCprg [1] . fmap read . splitOn ","

day09b :: String -> String
day09b = show . last . flip evalCGCprg [2] . fmap read . splitOn ","
