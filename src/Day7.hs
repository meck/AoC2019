module Day7 (day07a, day07b) where

import           CGC
import           Data.List                      ( permutations )
import           Util                           ( splitOn )

-- Runs the VMs with diffrent phase setings recursivly unil they all halted and
-- returns the last output of the last one
foldCGCs :: Foldable t => [Int] -> t Int -> Int
foldCGCs prog phas = last out
  where
    out = foldr (\p inp -> evalCGCprg prog (p : inp)) (0 : out) phas

-- Get the thrust for every phase setting
maxThrust :: [Int] -> [Int] -> Int
maxThrust p prg = maximum $ foldCGCs prg <$> permutations p

day07a :: String -> String
day07a = show . maxThrust [0 .. 4] . fmap read . splitOn ","

day07b :: String -> String
day07b = show . maxThrust [5 .. 9] . fmap read . splitOn ","
