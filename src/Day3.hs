module Day3 (day03a, day03b) where

import           Data.Map                       ( Map )
import qualified Data.Map                      as M
import           Data.List                      ( scanl' )
import           Data.List.Split                ( splitOn )
import           Util                           ( manhaDist )

type Cord = (Int, Int)

origin :: Cord
origin = (0, 0)

data Direction = U | L | D | R

expandStep :: String -> [Direction]
expandStep (d : lenStr) = case d of
    'U' -> rep U
    'L' -> rep L
    'D' -> rep D
    'R' -> rep R
    _   -> error "Bad Input"
    where rep = replicate (read lenStr)
expandStep _ = error "Bad Input"

move :: Cord -> Direction -> Cord
move (x, y) d = case d of
    U -> (succ x, y)
    L -> (x, pred y)
    D -> (pred x, y)
    R -> (x, succ y)

makeWire :: [String] -> Map Cord Int
makeWire steps = M.fromListWithKey (const min) $ zip cs [1 ..]
    where cs = tail $ scanl' move origin (steps >>= expandStep)

findIntersections :: [Map Cord Int] -> [Cord]
findIntersections m = M.keys $ foldr M.intersection (head m) (tail m)

solveA :: [[String]] -> Int
solveA = minimum . fmap (manhaDist origin) . findIntersections . fmap makeWire

solveB :: [[String]] -> Int
solveB input =
    minimum $ (\c -> sum $ fmap (M.! c) wires) <$> findIntersections wires
    where wires = makeWire <$> input

day03a :: String -> String
day03a = show . solveA . fmap (splitOn ",") . lines

day03b :: String -> String
day03b = show . solveB . fmap (splitOn ",") . lines
