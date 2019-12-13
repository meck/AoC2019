module Day13 (day13a, day13b) where

import           CGC
import           Control.DeepSeq                ( deepseq )
import           Data.List.Split                ( chunksOf )
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Data.Maybe                     ( catMaybes
                                                , fromMaybe
                                                )
import           Graphics.Gloss
import           Graphics.Gloss.Interface.Pure.Simulate
import           System.IO.Unsafe
import           Util                           ( splitOn )

type Cord = (Int, Int)

res :: Int
res = 20

winPos :: Cord
winPos = (10, 10)

speed :: Int
speed = 500

scoreHeight :: Int
scoreHeight = 2 * res

background :: Color
background = makeColorI 46 52 64 255

wallColor :: Color
wallColor = makeColorI 76 86 106 255

blockColor :: Color
blockColor = makeColorI 129 161 193 255

paddleColor :: Color
paddleColor = makeColorI 208 135 112 255

ballColor :: Color
ballColor = makeColorI 163 190 140 255

textColor :: Color
textColor = makeColorI 216 222 233 255

textBgColor :: Color
textBgColor = makeColorI 67 76 94 255

fRes :: Float
fRes = fromIntegral res

winSize :: Cord
winSize = (res * 45, res * 24 + scoreHeight)

trlAndSca :: Int -> Int -> Picture -> Picture
trlAndSca x y = translate
    (fromIntegral $ res * x - (fst winSize - res) `div` 2)
    (fromIntegral $ negate $ res * y - (snd winSize - res) `div` 2 + scoreHeight
    )

data TileID = Empty | Wall | Block deriving (Enum, Show, Eq)
type Tiles = Map Cord TileID
data Joystick = TiltLeft | TiltRight | Straight deriving (Show, Eq)

getJoy :: Joystick -> Int
getJoy TiltLeft  = -1
getJoy TiltRight = 1
getJoy Straight  = 0

data Sim = Sim { tiles      :: Tiles
                 , ball     :: Maybe Cord
                 , paddle   :: Maybe Cord
                 , score    :: Int
                 , joystick :: Joystick
                 , cpu      :: CGCState } deriving Eq

drawTileID :: TileID -> Picture
drawTileID Empty = Blank
drawTileID Wall  = color wallColor $ rectangleSolid fRes fRes
drawTileID Block = color blockColor $ thickCircle (fRes * 0.4) (fRes * 0.05)

drawTile :: Cord -> TileID -> Picture
drawTile (x, y) = trlAndSca x y . drawTileID

drawPaddle :: Cord -> Picture
drawPaddle (x, y) =
    trlAndSca x y $ color paddleColor $ rectangleSolid fRes fRes

drawBall :: Cord -> Picture
drawBall (x, y) = trlAndSca x y $ color ballColor $ circleSolid (fRes / 2)

drawScore :: Int -> Picture
drawScore s = pictures [bkg, sp]
  where
    sp =
        translate 0 (fromIntegral $ scoreHeight `div` 2)
            $  trlAndSca 0 0
            $  scale (fRes / 100) (fRes / 100)
            $  color textColor
            $  text
            $  "Score: "
            <> show s
    bkg =
        translate 0 (fromIntegral $ snd winSize `div` 2 - scoreHeight `div` 2)
            $ color textBgColor
            $ rectangleSolid (fromIntegral $ fst winSize)
                             (fromIntegral scoreHeight)

drawSim :: Sim -> Picture
drawSim w = pictures $ catMaybes
    [ Just $ pictures $ fmap (uncurry drawTile) $ M.toList $ tiles w
    , Just $ drawScore $ score w
    , drawPaddle <$> paddle w
    , drawBall <$> ball w
    ]

initalSim :: CGCState -> Sim
initalSim cgc = Sim
    { tiles    = M.empty
    , ball     = Nothing
    , paddle   = Nothing
    , score    = 0
    , joystick = Straight
    , cpu      = cgc
    }

data Inst = TilePos Cord TileID | BallPos Cord | PaddPos Cord | Score Int

readInst :: [Int] -> Inst
readInst is = case is of
    [s, 0, (-1)] -> Score s
    [0, y, x   ] -> TilePos (x, y) Empty
    [1, y, x   ] -> TilePos (x, y) Wall
    [2, y, x   ] -> TilePos (x, y) Block
    [3, y, x   ] -> PaddPos (x, y)
    [4, y, x   ] -> BallPos (x, y)
    _            -> error "Bad instruction"

stepSim :: ViewPort -> Float -> Sim -> Sim
stepSim vp t w = case runCGCuntilOut 3 (cpu w) ([getJoy (joystick w)], []) of
    Nothing             -> w
    Just (cpu', (_, o)) -> case readInst o of
        Score s           -> stepSim vp t $ w { score = s, cpu = cpu' }
        TilePos cord tile -> stepSim vp t
            $ w { tiles = M.insert cord tile (tiles w), cpu = cpu' }
        PaddPos c -> w { paddle = Just c, cpu = cpu' }
        BallPos c -> moveJoy $ w { ball = Just c, cpu = cpu' }

moveJoy :: Sim -> Sim
moveJoy w = w { joystick = nextJoy }
  where
    nextJoy = fromMaybe Straight $ do
        (px, _) <- paddle w
        (bx, _) <- ball w
        pure $ case px `compare` bx of
            GT -> TiltLeft
            LT -> TiltRight
            EQ -> Straight

runSim :: String -> IO ()
runSim inp = simulate (InWindow "Christmas Arcade" winSize winPos)
                      background
                      speed
                      (initalSim $ initCGC prog)
                      drawSim
                      stepSim
    where prog = (:) 2 $ tail $ read <$> splitOn "," inp

-- Just figure out the number of tiles
day13a :: String -> String
day13a =
    show
        . length
        . filter (== 2)
        . fmap last
        . chunksOf 3
        . flip evalCGCprg []
        . fmap read
        . splitOn ","

-- For running without the visualization
-- runSimWithoutDisplay :: [Char] -> Int
-- runSimWithoutDisplay prg = score $ fromJust $ firstEqual $ iterate
--     (stepSim undefined 0)
--     iSim
--     where iSim = initalSim $ initCGC $ (:) 2 $ tail $ read <$> splitOn "," prg

-- firstEqual :: Eq p => [p] -> Maybe p
-- firstEqual (x : y : _) | x == y = Just x
-- firstEqual (_ : xs)             = firstEqual xs
-- firstEqual _                    = Nothing

day13b :: String -> String
day13b = deepseq =<< unsafePerformIO . runSim
