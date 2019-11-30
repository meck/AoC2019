module Main (main) where

import AoC2019
import System.Environment (getArgs)
import System.Exit (die)

main :: IO ()
main = do
    args <- getArgs
    if null args then sequence_ $ solvePart <$> argLookup
                 else let n = head args in
                          case lookup n argLookup of
                          Nothing -> die $ n ++ " is not a valid exercise"
                          Just p -> solvePart (n,p)
                    

solvePart :: (String, String -> String) -> IO ()
solvePart (name, solu) = readFile (inputFp name) >>= putStrLn . (++) ("Solution to " ++ name ++ " is:\n") . solu
