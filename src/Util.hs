module Util
    ( strip
    , linesStrip
    , Data.List.Split.splitOn
    , Data.List.group
    , mostFreq
    , manhaDist
    , digitsR
    , digits
    , Data.Bifunctor.bimap
    , bimap'
    , (-^)
    , (+^)
    , groupSortOn
    )
where

import           Control.Arrow                  ( (&&&) )
import           Data.Bifunctor
import           Data.Function                  ( on )
import           Data.List                      ( group
                                                , groupBy
                                                , maximumBy
                                                , sort
                                                , sortBy
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Ord                       ( comparing )
import qualified Data.Text                     as T


bimap' :: Bifunctor p => (a -> d) -> p a a -> p d d
bimap' f = bimap f f

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
digitsR x = let (rest, lastDigit) = quotRem x 10 in lastDigit : digitsR rest

digits :: Integral n => n -> [n]
digits = reverse . digitsR

(+^) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(+^) (ax, ay) (bx, by) = (ax + bx, ay + by)

(-^) :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
(-^) (ax, ay) (bx, by) = (ax - bx, ay - by)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f =
    map (map snd) . groupBy ((==) `on` fst) . sortBy (compare `on` fst) . map
        (f &&& id)
