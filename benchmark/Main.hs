module Main (main) where

import           AoC2019
import           Gauge.Main

makeTests :: (String, String -> b) -> Benchmark
makeTests (name, f) =
    env (readInput name) $ \input -> bench name $ whnf f input

main :: IO ()
main = defaultMain $ makeTests <$> argLookup
