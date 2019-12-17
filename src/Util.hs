module Util
    ( strip
    , linesStrip
    , Data.List.Split.splitOn
    , Data.List.group
    , mostFreq
    , manhaDist
    , digitsR
    , digits
    , unDigits
    , Data.Bifunctor.bimap
    , bimap'
    , (-^)
    , (+^)
    , groupSortOn
    , drawCords
    , genGrid
    , firstEq
    )
where

import           Control.Applicative            ( liftA2 )
import           Control.Arrow                  ( (&&&) )
import           Control.Monad                  ( ap )
import           Data.Bifunctor
import           Data.Function                  ( on )
import           Data.List                      ( group
                                                , groupBy
                                                , maximumBy
                                                , sort
                                                , sortBy
                                                )
import           Data.List.Split                ( splitOn )
import qualified Data.Map.Lazy                 as M
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
digitsR 0 = [0]
digitsR n = go n
  where go 0 = []
        go x = let (rest, lastDigit) = quotRem x 10 in lastDigit : go rest

unDigits :: Integral n => [n] -> n 
unDigits = foldl ((+) . (10 *)) 0

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

-- The first in a list where the next one is equal
firstEq :: Eq a => [a] -> a
firstEq = fst . head . dropWhile (liftA2 (/=) snd fst) . ap zip tail

-- [[(0,0),(0,1)],[(1,0),(1,1)]]
genGrid :: (Enum b1, Enum a) => ((a, b1) -> b2) -> a -> a -> b1 -> b1 -> [[b2]]
genGrid f xMin xMax yMin yMax =
    fmap f <$> [ [ (x, y) | x <- [xMin .. xMax] ] | y <- [yMin .. yMax] ]


-- Draw a grid with a supplied (a -> Char) and a defualt `a`
drawCords
    :: (Ord b, Num b, Ord a1, Num a1, Enum b, Enum a1)
    => a2
    -> (a2 -> Char)
    -> M.Map (a1, b) a2
    -> String
drawCords def f m = unlines $ fmap (f . flip (M.findWithDefault def) m) <$> cs
  where
    xMin = M.foldrWithKey (\(x, _) _ x' -> min x x') 0 m
    xMax = M.foldrWithKey (\(x, _) _ x' -> max x x') 0 m
    yMin = M.foldrWithKey (\(_, y) _ y' -> min y y') 0 m
    yMax = M.foldrWithKey (\(_, y) _ y' -> max y y') 0 m
    cs   = [ [ (x, y) | x <- [xMin .. xMax] ] | y <- [yMin .. yMax] ]
