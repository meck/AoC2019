module Day16 (day16a, day16b) where

import           Data.List                      ( scanl' )
import           Util                           ( strip
                                                , unDigits
                                                )

oneD :: Integral a => a -> a
oneD x = abs x `rem` 10

step :: Integral a => [a] -> Int -> a
step inp =
    oneD
        . abs
        . sum
        . zipWith (*) inp
        . tail
        . cycle
        . ([0, 1, 0, -1] >>=)
        . replicate

fft :: Integral a => [a] -> [a]
fft is = iterate go is !! 100 where go = flip fmap [1 .. length is] . step

solveB :: String -> [Int]
solveB inpS | offset > length inp `div` 2 =
    take 8 $ reverse $ (!! 100) $ iterate addAndOneD $ reverse offInp
  where
    inp        = parse inpS
    offset     = unDigits $ take 7 inp
    offInp     = drop offset $ concat $ replicate 10000 inp
    addAndOneD = scanl' (\a b -> oneD (a + b)) 0
solveB _ = error "Not implemented"

parse :: String -> [Int]
parse = fmap (read . (: [])) . strip

day16a :: String -> String
day16a = show . unDigits . take 8 . fft . parse

day16b :: String -> String
day16b = show . unDigits . solveB
