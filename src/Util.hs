module Util
    ( strip
    , linesStrip
    , Data.List.Split.splitOn
    , Data.List.group
    , mostFreq
    , manhaDist
    , digitsR
    , digits
    )
where

import           Data.List                      ( group
                                                , maximumBy
                                                , sort
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Ord                       ( comparing )
import qualified Data.Text                     as T

strip :: String -> String
strip = T.unpack . T.strip . T.pack

linesStrip :: String -> [String]
linesStrip = fmap (T.unpack . T.strip) . T.lines . T.strip . T.pack

mostFreq :: Ord a => [a] -> [a]
mostFreq = maximumBy (comparing length) . group . sort

manhaDist :: Num a => (a, a) -> (a, a) -> a
manhaDist (x, y) (x', y') = abs (x - x') + abs (y - y')

digitsR :: Integral n => n -> [n]
digitsR 0 = []
digitsR x =
    let (rest, lastDigit) = quotRem x 10 in lastDigit : digitsR rest

digits :: Integral n => n -> [n]
digits = reverse . digitsR
