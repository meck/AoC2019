{-# LANGUAGE TupleSections #-}
module Day15 (day15a, day15b) where

import           CGC
import qualified Data.Map.Lazy                 as M
import           Data.Maybe                     ( fromJust
                                                , isNothing
                                                )
import qualified Parsing                       as P
import qualified Queue                         as Q
import           Util                           ( (+^) )

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

data Step = Step { rep :: Reply
                 , sta :: CGCState
                 , pos :: (Int,Int)
                 } deriving (Eq, Show)

instance Ord Step where
  compare s s' = pos s `compare` pos s'

queryCGC :: CGCState -> Direction -> (Reply, CGCState)
queryCGC c d = case runCGCuntilOut 1 c ([fromEnum d], []) of
    Right (c', (_, [o])) -> (toEnum o, c')
    _                    -> error "Incorrect Output or halt"

bfsWhile :: Ord k => (k -> Maybe [k]) -> k -> M.Map k Int
bfsWhile gen inital = go M.empty (Q.push (0, inital) Q.empty)
  where
    go seen q
        | Q.isEmpty q = seen
        | k `M.member` seen = go seen q'
        | isNothing mNext = M.insert k d seen
        | otherwise = go (M.insert k d seen)
        $ foldr (Q.push . (succ d, )) q'
        $ fromJust mNext
      where
        (mk, q') = Q.pop q
        (d , k ) = fromJust mk
        mNext    = gen k

nextSteps :: Step -> Maybe [Step]
nextSteps (Step Oxy  _ _) = Nothing
nextSteps (Step Wall _ _) = Just []
nextSteps stp             = Just $ takeStep <$> [minBound .. maxBound]
  where
    takeStep d = uncurry Step (queryCGC (sta stp) d) $ case d of
        N -> pos stp +^ (0, 1)
        S -> pos stp +^ (0, -1)
        W -> pos stp +^ (-1, 0)
        E -> pos stp +^ (1, 0)

solveA :: [Int] -> (Step, Int)
solveA inp = M.findMax $ M.filterWithKey (const . (Oxy ==) . rep) $ bfsWhile
    nextSteps
    (Step Moved (initCGC inp) (0, 0))

solveB :: [Int] -> Int
solveB inp = M.foldr max 0 $ bfsWhile nextSteps (oxy { rep = Moved })
    where oxy = fst $ solveA inp

day15a :: String -> String
day15a = show . snd . solveA . P.run (P.commaSepListOf P.integer)

day15b :: String -> String
day15b = show . solveB . P.run (P.commaSepListOf P.integer)
