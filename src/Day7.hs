module Day7 (day07a, day07b) where

import           CGC
import Util (splitOn)
import Data.List (permutations)

-- solveA ip = maximum $ testPha ip <$> phaSet


-- testPha prg pha = compE
--   where compA = runCGC $ initCGC prg $ (pha !! 0) : [0]
--         compB = runCGC $ initCGC prg $ (pha !! 1) : compA
--         compC = runCGC $ initCGC prg $ (pha !! 2) : compB
--         compD = runCGC $ initCGC prg $ (pha !! 3) : compC
--         compE = runCGC $ initCGC prg $ (pha !! 4) : compD

-- phaSet = permutations [5..9]

day07a :: String -> String
day07a = id -- show . solveA . fmap read . splitOn ","
-- day07a = id

day07b :: String -> String
day07b = id

