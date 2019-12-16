module Day15 (day15a, day15b) where

import           CGC
import           Control.Monad.AStar
import           Control.Monad.State.Strict
import           Data.Foldable                  ( asum )
import           Data.Maybe                     ( fromJust )
import           Data.Monoid                    ( Sum(..) )
import qualified Parsing                       as P

data Direction = N | S | W | E deriving (Eq, Bounded, Show)

instance Enum Direction where
    toEnum i = case i of
        1 -> N
        2 -> S
        3 -> W
        4 -> E
        _ -> error "Not a direction"
    fromEnum d = case d of
        N -> 1
        S -> 2
        W -> 3
        E -> 4

data Reply = Wall | Moved | Oxy deriving (Eq, Enum, Ord, Show)

type SearchTank a = AStar (CGCState, [Direction]) (Sum Int) Reply a

queryCGC :: CGCState -> Direction -> (Reply, CGCState)
queryCGC c d = case runCGCuntilOut 1 c ([fromEnum d], []) of
    Right (c', (_, [o])) -> (toEnum o, c')
    _                    -> error "Incorrect Output or halt"

stepAstar :: Direction -> SearchTank ()
stepAstar d = do
    spend 1
    (cpu, ds) <- get
    let (r, cpu') = queryCGC cpu d
    put (cpu', d : ds)
    case r of
        Wall  -> failure
        Oxy   -> done Oxy
        Moved -> asum $ stepAstar <$> nextSteps
  where
    nextSteps = filter (/= antiD) [minBound .. maxBound]
    antiD     = case d of
        N -> S
        S -> N
        W -> E
        E -> W

solveA :: [Int] -> Int
solveA inp = fromJust $ length . snd <$> execAStar
    (asum $ stepAstar <$> [minBound .. maxBound])
    (initCGC inp, [])

day15a :: String -> String
day15a = show . solveA . P.run (P.commaSepListOf P.integer)

day15b :: String -> String
day15b = id
