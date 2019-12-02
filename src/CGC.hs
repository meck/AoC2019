module CGC (runCGC, runCGCwithVandN) where

-- CHC: Christmas Guidence Computer!

import           Control.Monad.State.Lazy
import           Control.Applicative            ( liftA2 )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Data.Maybe                     ( fromMaybe )

type CGCState = State CGC

data CGC = CGC { mem :: Vector Int,
                  ip :: Int } deriving (Eq, Show)

mult :: CGCState ()
mult = mathOp (*)

add :: CGCState ()
add = mathOp (+)


exit :: CGCState Int
exit = V.head . mem <$> get

-- Helpers
moveIp :: Int -> CGCState ()
moveIp i = do
    s <- get
    put $ s { ip = ip s + i }

mathOp :: (Int -> Int -> Int) -> CGCState ()
mathOp f = do
    i  <- ip <$> get
    x  <- getAddr (i + 1) >>= getAddr
    y  <- getAddr (i + 2) >>= getAddr
    d' <- getAddr $ i + 3
    setAddr d' (x `f` y)
    moveIp 4

verbAndNoun :: Int -> Int -> CGCState ()
verbAndNoun v n = do
    setAddr 1 v
    setAddr 2 n

getAddr :: Int -> CGCState Int
getAddr addr =
    fromMaybe (error "CGC Error: Memory out bounds") . (V.!? addr) . mem <$> get


setAddr :: Int -> Int -> CGCState ()
setAddr addr val = do
    s <- get
    put $ s { mem = mem s V.// [(addr, val)] }

-- Api
step :: CGCState Int
step = do
    inst <- liftA2 (V.!?) mem ip <$> get
    case inst of
        Just 1  -> add >> step
        Just 2  -> mult >> step
        Just 99 -> exit
        Just _  -> error "CGC Error: Invalid Instruction"
        Nothing -> error "CGC Error: Instruction Poiner out of bounds"

runCGC :: [Int] -> Int
runCGC m = evalState step $ CGC (V.fromList m) 0

runCGCwithVandN :: [Int] -> (Int, Int) -> Int
runCGCwithVandN m (v, n) =
    evalState (verbAndNoun v n >> step) $ CGC (V.fromList m) 0
