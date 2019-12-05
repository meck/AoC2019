module CGC (CGC(..), runCGC, runCGC_, initCGC, setVerbAndNoun, readMem) where

-- CGC: Christmas Guidance Computer!

import           Control.Monad.State.Lazy
import           Data.Maybe                     ( fromMaybe )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Util                           ( digitsR )

type CGCState = State CGC

data CGC = CGC { mem      :: Vector Int,
                 iP       :: Int,
                inputBuf  :: [Int],
                outputBuf :: [Int] } deriving (Eq, Show)

newtype Pointer = Pointer { getPtr :: Int } deriving (Show, Eq)

data Param = Ptr Int | Val Int deriving (Show, Eq)

data Op = ADD Param Param Pointer
        | MUL Param Param Pointer
        | READ Pointer
        | WRITE Param
        | JZ Param Param
        | JNZ Param Param
        | LESS Param Param Pointer
        | CMP Param Param Pointer
        | HALT
            deriving (Eq, Show)

doOp :: Op -> CGCState ()

doOp (ADD x y r) = mathOp (+) x y r

doOp (MUL x y r) = mathOp (*) x y r

doOp (READ d   ) = do
    s <- get
    case inputBuf s of
        []       -> error "CGC Error: Missing Input"
        (i : is) -> setInpBuf is >> setAdr d i >> moveIP 2

doOp (WRITE m) = do
    m' <- derefPar m
    s  <- get
    put $ s { outputBuf = m' : outputBuf s }
    moveIP 2

doOp (JZ x d) = derefPar x >>= (\x' -> if x' == 0 then setIP d else moveIP 3)

doOp (JNZ x d) = derefPar x >>= (\x' -> if x' /= 0 then setIP d else moveIP 3)

doOp (LESS x y d) = do
    x' <- derefPar x
    y' <- derefPar y
    (if x' < y' then setAdr d 1 else setAdr d 0) >> moveIP 4

doOp (CMP x y d) = do
    x' <- derefPar x
    y' <- derefPar y
    (if x' == y' then setAdr d 1 else setAdr d 0) >> moveIP 4

doOp HALT = pure ()

-- Helpers
mathOp :: (Int -> Int -> Int) -> Param -> Param -> Pointer -> CGCState ()
mathOp f x y d = do
    x' <- derefPar x
    y' <- derefPar y
    setAdr d (x' `f` y')
    moveIP 4

derefPar :: Param -> CGCState Int
derefPar (Val i) = pure i
derefPar (Ptr p) = derefPtr (Pointer p)

derefPtr :: Pointer -> CGCState Int
derefPtr (Pointer addr) =
    fromMaybe (error "CGC Error: Memory access out of bounds")
        .   (V.!? addr)
        .   mem
        <$> get

setAdr :: Pointer -> Int -> CGCState ()
setAdr (Pointer addr) val = modify $ \s -> s { mem = mem s V.// [(addr, val)] }

setIP :: Param -> CGCState ()
setIP i = do
    i' <- derefPar i
    s  <- get
    put $ s { iP = i' }

moveIP :: Int -> CGCState ()
moveIP i = modify $ \s -> s { iP = iP s + i }

setInpBuf :: [Int] -> CGCState ()
setInpBuf i = modify $ \s -> s { inputBuf = i }


-- Tick Tock
nextInst :: CGC -> Op
nextInst cgc =
    let op  = fromMaybe (error "CGC Error: IP out of bounds") $ m V.!? i
        pa0 = par 0
        pa1 = par 1
        po0 = ptr 0
        po2 = ptr 2
    in  case digPad 2 op of
            [1, 0] -> ADD pa0 pa1 po2
            [2, 0] -> MUL pa0 pa1 po2
            [3, 0] -> READ po0
            [4, 0] -> WRITE pa0
            [5, 0] -> JNZ pa0 pa1
            [6, 0] -> JZ pa0 pa1
            [7, 0] -> LESS pa0 pa1 po2
            [8, 0] -> CMP pa0 pa1 po2
            [9, 9] -> HALT
            _      -> error $ "Invalid CGC  Instruction: " <> show op
  where
    par n = fromMaybe (error "Parameter error") $ do
        t <- last . digPad (n + 3) <$> (m V.!? i)
        case t of
            0 -> Ptr <$> (m V.!? (succ i + n))
            1 -> Val <$> (m V.!? (succ i + n))
            _ -> Nothing
    ptr n = maybe (error "Parameter error") Pointer (m V.!? (succ i + n))
    i = iP cgc
    m = mem cgc

-- API

-- Run and return the final state
runCGC_ :: CGC -> ((), CGC)
runCGC_ = runState step
  where
    step = do
        s <- get
        case nextInst s of
            HALT -> doOp HALT
            i    -> doOp i >> step

-- Run and return the output
runCGC :: CGC -> [Int]
runCGC = outputBuf . snd . runCGC_

-- Setup with memory and Inputbuffer
initCGC :: [Int] -> [Int] -> CGC
initCGC m inp = CGC (V.fromList m) 0 inp []

-- Set the Verb and Noun before running
setVerbAndNoun :: (Int, Int) -> (CGC -> CGC)
setVerbAndNoun (v, n) cgc = cgc { mem = mem cgc V.// [(1, v), (2, n)] }

readMem :: CGC -> [Int]
readMem cgc = V.toList $ mem cgc

digPad :: Integral a => Int -> a -> [a]
digPad n i = take n $ digitsR i <> repeat 0
