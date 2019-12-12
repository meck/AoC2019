module Day10 (day10a, day10b) where

import           Data.List                      ( sortOn
                                                , transpose
                                                )
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Util                           ( groupSortOn
                                                , manhaDist
                                                )

type Cord = (Integer, Integer)

type AstMap = Set Cord

-- Get the angle between A B (0 to 2pi) 0 north
angle :: Cord -> Cord -> Double
angle (xA, yA) (xB, yB) = (\a -> if a < 0 then a + 2 * pi else a) $ atan2 dx dy
  where
    dx = fromInteger $ xB - xA
    dy = fromInteger $ yA - yB

-- Number of seen asteroids from a point
-- using fusion of the set
noOfseen :: AstMap -> Cord -> Int
noOfseen am c = S.size $ S.map (angle c) am

-- Cord with most visible asteroids
findBest :: AstMap -> (Int, Cord)
findBest m = S.findMax $ S.map ((,) =<< noOfseen m) m

-- The list of vaporizing
vapeList :: Cord -> AstMap -> [Cord]
vapeList c =
    concat
        . transpose
        . fmap (sortOn (manhaDist c))
        . groupSortOn (angle c)
        . filter (/= c)
        . S.toList

parse :: String -> AstMap
parse inp = S.fromList $ snd <$> filter ((== '#') . fst) cord
  where
    h    = toInteger $ length $ lines inp
    w    = toInteger $ length $ head $ lines inp
    cord = zip (concat $ lines inp)
               [ (x, y) | y <- [0 .. pred h], x <- [0 .. pred w] ]

day10a :: String -> String
day10a = show . fst . findBest . parse

day10b :: String -> String
day10b inp = show $ uncurry ((+) . (100 *)) $ (!! 199) $ vapeList base astMap
  where
    astMap = parse inp
    base   = snd $ findBest astMap
