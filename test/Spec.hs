{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           AoC2019
import           Test.Tasty
import           Test.Tasty.HUnit
import           Data.List
import qualified Data.Text                     as T
import           Prelude                 hiding ( readFile )
import           Data.Text.IO                   ( readFile )
import           Data.Bifunctor                 ( bimap )


getTests :: String -> IO [(String, String)]
getTests name = do
    solu <- readFile $ "test-data/" ++ name ++ ".txt"
    pure $ case solu of
        "" -> []
        _ ->
            bimap (T.unpack . T.strip) (T.unpack . T.strip . T.drop 5)
                .   T.breakOn ">>>> "
                <$> T.splitOn "<<<<\n" (T.strip solu)

makeTest :: (Eq a, Show a) => ((String, t -> a), [(t, a)]) -> TestTree
makeTest ((name, solu), tests) =
    testGroup name
        $ (\((tInp, tOut), i) -> testCase (show (i :: Int)) $ solu tInp @?= tOut
          )
        <$> zip tests [1 ..]


main :: IO ()
main = do
    testFiles <- sequenceA $ getTests . fst <$> argLookup
    defaultMain $ testGroup "AoC2019" $ makeTest <$> zip argLookup testFiles
