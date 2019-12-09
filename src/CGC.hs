module CGC
    ( CGCState(..)
    , CGC
    , Buffers
    , Result
    , evalCGC
    , execCGC
    , runCGC
    , initCGC
    , setVerbAndNoun
    , readMem
    )
where

-- CGC: Christmas Guidance Computer!

import           Control.Monad.State.Lazy
import           Data.Maybe                     ( fromMaybe )
import           Data.Vector                    ( Vector )
import qualified Data.Vector                   as V
import           Util                           ( digitsR )

-- | The 'Effect' type contains all the primitive effects that are
-- observable on the virtual machine.
data Effect a =
    Done a
  | ReadInt (Int -> Effect a)
  | Print Int (Effect a)
  | Fail String

instance Functor Effect where
    fmap f (Done     x) = Done (f x)
    fmap f (ReadInt t) = ReadInt (fmap f . t)
    fmap f (Print c t ) = Print c (fmap f t)
    fmap _ (Fail msg  ) = Fail msg

instance Applicative Effect where
    pure  = Done
    (<*>) = ap

instance Monad Effect where
    return = Done
    (Done     x) >>= f = f x
    (ReadInt t) >>= f = ReadInt (t >=> f)
    (Print c t ) >>= f = Print c (t >>= f)
    (Fail msg  ) >>= _ = Fail msg

instance Eq a => Eq (Effect a) where
    (Done x) == (Done y) = x == y
    (ReadInt f) == (ReadInt g) =
        all (\x -> f x == g x) [minBound .. maxBound]
    (Print c t) == (Print d u) = c == d && t == u
    (Fail s   ) == (Fail t   ) = s == t
    _           == _           = False

type CGC a = StateT CGCState Effect a

data CGCState = CGCState { mem      :: Vector Int,
                           iP       :: Int } deriving (Eq, Show)

readInt :: CGC Int
readInt = StateT (\s -> ReadInt (\c -> Done (c, s)))

printInt :: Int -> CGC ()
printInt c = StateT (\s -> Print c (Done ((), s)))

cgcFail :: String -> CGC a
cgcFail str = StateT (\_ -> Fail str)

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

doOp :: Op -> CGC ()
doOp (ADD x y r ) = mathOp (+) x y r
doOp (MUL x y r ) = mathOp (*) x y r
doOp (READ  d   ) = (readInt >>= setAdr d) >> moveIP 2
doOp (WRITE m   ) = (derefPar m >>= printInt) >> moveIP 2
doOp (JZ x d) = derefPar x >>= (\x' -> if x' == 0 then setIP d else moveIP 3)
doOp (JNZ x d) = derefPar x >>= (\x' -> if x' /= 0 then setIP d else moveIP 3)
doOp (LESS x y d) = do
    x' <- derefPar x
    y' <- derefPar y
    if x' < y' then setAdr d 1 else setAdr d 0
    moveIP 4
doOp (CMP x y d) = do
    x' <- derefPar x
    y' <- derefPar y
    if x' == y' then setAdr d 1 else setAdr d 0
    moveIP 4
doOp HALT = pure ()

-- Helpers
mathOp :: (Int -> Int -> Int) -> Param -> Param -> Pointer -> CGC ()
mathOp f x y d = do
    x' <- derefPar x
    y' <- derefPar y
    setAdr d (x' `f` y')
    moveIP 4

derefPar :: Param -> CGC Int
derefPar (Val i) = pure i
derefPar (Ptr p) = derefPtr (Pointer p)

derefPtr :: Pointer -> CGC Int
derefPtr (Pointer addr) = do
    st <- get
    let mi = mem st V.!? addr
    case mi of
        Just i  -> pure i
        Nothing -> cgcFail "Memory access out of bounds"

setAdr :: Pointer -> Int -> CGC ()
setAdr (Pointer addr) val = modify $ \s -> s { mem = mem s V.// [(addr, val)] }

setIP :: Param -> CGC ()
setIP i = do
    i' <- derefPar i
    s  <- get
    put $ s { iP = i' }

moveIP :: Int -> CGC ()
moveIP i = modify $ \s -> s { iP = iP s + i }


nextInst :: CGCState -> Op
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
    digPad n int = take n $ digitsR int <> repeat 0

-- API

-- Run withour input and ignore output
-- untill halt and return the final state
execCGC :: CGCState -> CGCState
execCGC s = case runCGC s ([], []) of
    Halted  (_, s') -> s'
    Waiting _       -> error "CGC Error: No input"


-- Run till halt and return all the output
-- in order of they were produced
evalCGC :: CGCState -> [Int] -> [Int]
evalCGC s inp = case runCGC s (inp, []) of
    Halted  ((_, o), _) -> o
    Waiting _           -> error "CGC Error: No input"

type Buffers = ([Int],[Int])

data Result = Halted (Buffers,CGCState) | Waiting (Buffers, CGCState)

runCGC :: CGCState -> Buffers -> Result
runCGC s buf = handleEff buf $ runStateT (get >>= doOp . nextInst) s
  where
    handleEff (i, o) (ReadInt f) =
        if null i then Waiting (buf, s) else handleEff (tail i, o) (f $ head i)
    handleEff (i, o) (Print p eff ) = handleEff (i, p : o) eff
    handleEff _      (Fail str    ) = error $ "CGC Error: " <> str
    handleEff (i, o) (Done (_, c')) = case nextInst c' of
        HALT -> Halted ((i, o), c')
        _    -> runCGC c' (i, o)

-- Setup with memory
initCGC :: [Int] -> CGCState
initCGC m = CGCState (V.fromList m) 0

-- Set the Verb and Noun
setVerbAndNoun :: (Int, Int) -> (CGCState -> CGCState)
setVerbAndNoun (v, n) cgc = cgc { mem = mem cgc V.// [(1, v), (2, n)] }

-- Get the memory
readMem :: CGCState -> [Int]
readMem cgc = V.toList $ mem cgc
