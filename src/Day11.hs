{-# LANGUAGE LambdaCase #-}
module Day11 (day11a, day11b) where

import           CGC
import           Control.Monad.State.Lazy
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Util                           ( splitOn
                                                , (+^)
                                                , drawCords
                                                )

type Cord = (Integer, Integer)

data Direction = U | R | D | L deriving (Enum, Bounded, Eq, Show)

turn :: Direction -> Integer -> Direction
turn d = \case
    0 -> prev d
    1 -> next d
    _ -> error "Bad direction"
  where
    prev a | a == minBound = maxBound
    prev a                 = pred a
    next a | a == maxBound = minBound
    next a                 = succ a

dirDelta :: Direction -> Cord
dirDelta = \case
    U -> (0, -1)
    R -> (1, 0)
    D -> (0, 1)
    L -> (-1, 0)

data Color = Black | White deriving (Enum, Show)

dispColor :: Color -> Char
dispColor Black = ' '
dispColor White = '█'

data PaintState = PaintState { robDir :: Direction
                             , panels :: Map Cord Color
                             , robPos :: Cord
                             , robSta :: CGCState }

initState :: [Int] -> PaintState
initState prg = PaintState { robDir = U
                           , panels = M.empty
                           , robPos = (0, 0)
                           , robSta = initCGC prg
                           }

type Paint a = State PaintState a

findColor :: Cord -> Map Cord Color -> Color
findColor = M.findWithDefault Black

getColor :: Paint Color
getColor = (\x -> findColor (robPos x) (panels x)) <$> get

paintPanel :: Color -> Paint ()
paintPanel c = modify (\s -> s { panels = M.insert (robPos s) c (panels s) })

stepRobot :: Paint ()
stepRobot = modify (\s -> s { robPos = robPos s +^ dirDelta (robDir s) })

turnRobot :: Integer -> Paint ()
turnRobot d = modify (\s -> s { robDir = turn (robDir s) d })

updateRob :: CGCState -> Paint ()
updateRob st = modify (\s -> s { robSta = st })

nextInst :: Paint (Maybe (Color, Integer))
nextInst = do
    cc <- getColor
    st <- robSta <$> get
    let a = runCGCuntilOut 2 st ([fromEnum cc], [])
    case a of
        Right (st', (_, [d, c])) -> do
            updateRob st'
            pure $ Just (toEnum c, toInteger d)
        _ -> pure Nothing

runRobot :: Paint (Map Cord Color)
runRobot = do
    mcd <- nextInst
    case mcd of
        Just (c, d) -> paintPanel c *> turnRobot d *> stepRobot *> runRobot
        Nothing     -> panels <$> get

day11a :: String -> String
day11a =
    show . M.size . evalState runRobot . initState . fmap read . splitOn ","

day11b :: String -> String
day11b =  drawCords Black dispColor
        . evalState (paintPanel White *> runRobot)
        . initState
        . fmap read
        . splitOn ","
