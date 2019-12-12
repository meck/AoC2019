module Day12 (day12a, day12b) where

import           Data.Char                      ( isDigit )
import           Data.List                      ( findIndex )
import           Data.Maybe                     ( fromJust )
import Util (splitOn)
newtype Pos = Pos { pos :: (Int,Int,Int)} deriving (Eq, Show)
newtype Vel = Vel { vel :: (Int,Int,Int)} deriving (Eq, Show)
newtype Acc = Acc { acc :: (Int,Int,Int)} deriving (Eq, Show)

data Moon = Moon { p :: Pos
                 , v :: Vel} deriving (Eq, Show)

initMoon :: Int -> Int -> Int -> Moon
initMoon x y z = Moon (Pos (x, y, z)) (Vel (0, 0, 0))

apAcc :: Vel -> Acc -> Vel
apAcc = (Vel .) . (. acc) . (+*) . vel

apVel :: Pos -> Vel -> Pos
apVel = (Pos .) . (. vel) . (+*) . pos

(+*) :: (Num a, Num b, Num c) => (a, b, c) -> (a, b, c) -> (a, b, c)
(+*) (a, b, c) (a', b', c') = (a + a', b + b', c + c')

calcAcc :: Pos -> Pos -> Acc
calcAcc (Pos (x, y, z)) (Pos (x', y', z')) = Acc
    (x `co` x', y `co` y', z `co` z')
  where
    co a b = case compare a b of
        LT -> 1
        GT -> -1
        EQ -> 0

applyGrav :: [Moon] -> [Moon]
applyGrav ms = applyGrav' <$> ms
  where
    applyGrav' m = m { v = v m `apAcc` totAcc m }
    totAcc m = Acc $ foldr ((+*) . acc . calcAcc (p m) . p)
                           (0, 0, 0)
                           (filter (/= m) ms)

applyVel :: Moon -> Moon
applyVel m = m { p = p m `apVel` v m }

totEnergy :: Moon -> Int
totEnergy (Moon (Pos (x, y, z)) (Vel (x', y', z'))) =
    (abs x + abs y + abs z) * (abs x' + abs y' + abs z')

step :: [Moon] -> [Moon]
step = fmap applyVel . applyGrav

parse :: String -> [Moon]
parse inp = step2
    where step1 = lines inp
          step2 = parse' . splitOn "," . filter (\c -> isDigit c || elem c "-,") <$> step1
          parse' (a:b:c:_) = initMoon (read a)  (read b)  (read c)
          parse' _ = error "Bad input"

day12a :: String -> String
day12a = show . sum . fmap totEnergy . (!! 1000) . iterate step . parse

-- Get the periodicity of each dimension and get the `lcm` of all of them
day12b :: String -> String
day12b inp = show $ lcm (period xDimen) $ lcm (period yDimen) (period zDimen)
  where period d = succ $ length $ takeWhile (/= head xs) $ tail xs
            where xs = fmap d <$> iterate step (parse inp)
        xDimen (Moon (Pos (px, _, _)) (Vel (vx, _, _))) = (px, vx)
        yDimen (Moon (Pos (_, py, _)) (Vel (_, vy, _))) = (py, vy)
        zDimen (Moon (Pos (_, _, pz)) (Vel (_, _, vz))) = (pz, vz)
