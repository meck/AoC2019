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
solvePart (name, solu) = do
    input <- readFile (inputFp name)
    case input of
        "" -> putStrLn $ "No input for puzzle " <> name
        _  -> putStrLn $ (<>) ("Solution to " <> name <> " is:\n") $ solu input
