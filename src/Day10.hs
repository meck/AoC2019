module Day10 (day10a, day10b) where

import           Data.List                      ( sortOn
                                                , unfoldr
                                                , groupBy
                                                , transpose
                                                , sort
                                                )
import           Data.Maybe                     ( mapMaybe )
import           Data.Ratio
import           Data.Set                       ( Set )
import qualified Data.Set                      as S
import           Data.Function                  ( on )
import           Util                           ( bimap'
                                                , strip
                                                , manhaDist
                                                , (-^)
                                                )

type Cord = (Integer, Integer)

type AstMap = Set Cord

-- The n'th order Farey sequence.
farey :: Integer -> [Rational]
farey n = 0 : unfoldr step (0, 1, 1, n)
  where
    step (a, b, c, d)
        | c > n
        = Nothing
        | otherwise
        = let k = (n + b) `quot` d
          in  Just (c % d, (c, d, k * c - a, k * d - b))

-- The point on a line a fraction `k` of the length
rationalPoint :: Num b => (b, b) -> (b, b) -> b -> (b, b)
rationalPoint (ax, ay) (bx, by) k = (ax + k * (bx - ax), ay + k * (by - ay))

-- All Integer points berween two points
interCords :: Cord -> Cord -> [Cord]
interCords a b =
    mapMaybe intCord $ 
    rationalPoint (bimap' toRational a) (bimap' toRational b)
        <$> frac
  where
    -- The number of segments of a line
    nSeg      = uncurry gcd $ a -^ b
    -- The fractions of those intersections
    -- excluding (0/1) and (1/1)
    frac      = filter (not . isInteger) $ farey nSeg
    isInteger = (1 ==) . denominator
    intCord c = if uncurry (&&) $ bimap' isInteger c
        then Just $ bimap' numerator c
        else Nothing

losClear :: AstMap -> Cord -> Cord -> Bool
losClear _ x y | x == y = False
losClear m x y          = not $ or $ flip S.member m <$> interCords x y

noOfseen :: AstMap -> Cord -> Int
noOfseen m c = length $ filter id $ fmap (losClear m c) (S.toList m)

findBest :: AstMap -> (Cord, Int)
findBest m = last $ sortOn snd $ zip l (noOfseen m <$> l) where l = S.toList m

parse :: String -> AstMap
parse inp = S.fromList $ snd <$> filter ((== '#') . fst) cord
  where
    h    = toInteger $ length $ lines inp
    w    = toInteger $ length $ head $ lines inp
    cord = zip (concat $ lines inp)
               [ (x, y) | y <- [0 .. pred h], x <- [0 .. pred w] ]

day10a :: String -> String
day10a = show . snd . findBest . parse


test am c@(cx,cy) =  e
    where a = groupOn ang $ filter (/=c) $ S.toList am
          b = sortOn (manhaDist c) <$> a
          d = sortOn (ang . head) b
          -- d = sortOn ( (\ang' -> if ang' < pi/2 then ang' - 2 *pi else ang') . ang . head) b
          e = concat $ transpose d
          ang = (\ang' -> if ang' < 0 then ang' + 2*pi else ang') . uncurry  atan2 . bimap' fromInteger . (\(ax,ay) -> (ax - cx, cy - ay))
          -- ang = uncurry  atan2 . bimap' fromInteger . (\(ax,ay) -> ( ax - cx ,cy - ay ))
          -- ang = uncurry atan2 . bimap' fromInteger . (-^ c)
          dist (ax,ay) (bx,by) = sqrt $ (fromInteger ax - fromInteger bx)**2 + (fromInteger ay - fromInteger by)**2

-- | A version of 'group' where the equality is done on some extracted value.
groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on2` f)
-- groupOn f = groupBy ((==) `on2` f)
    -- redefine on so we avoid duplicate computation for most values.
    where (.*.) `on2` f = \x -> let fx = f x in \y -> fx .*. f y

-- ang' = uncurry atan2 . bimap' fromInteger . ((2,3) -^)

day10b :: String -> String
-- day10b = id
day10b inp = show $ test p (fst $ findBest p)
    where p = parse $ strip $ inp
