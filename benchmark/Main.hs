module Main (main) where

import           Gauge.Main
import           AoC2019

makeTests :: (String, String -> b) -> Benchmark
makeTests (name, f) =
    env (readFile $ inputFp name) $ \input -> bench name $ whnf f input

main :: IO ()
main = defaultMain $ makeTests <$> argLookup
