module CGC
    ( CGCState(..)
    , evalCGC
    , execCGC
    , evalCGCprg
    , initCGC
    , setVerbAndNoun
    , readMem
    )
where

-- CGC: Christmas Guidance Computer!

import           Control.Monad.State.Lazy
import           Data.IntMap.Lazy               ( IntMap )
import qualified Data.IntMap.Lazy              as IM
import           Data.Maybe                     ( fromMaybe )
import           Util                           ( digitsR )

-- | The 'Effect' type contains all the primitive effects that are
-- observable on the virtual machine.
data Effect a =
    Done a
  | ReadInt (Int -> Effect a)
  | Print Int (Effect a)
  | Fail String

instance Functor Effect where
    fmap f (Done    x) = Done (f x)
    fmap f (ReadInt t) = ReadInt (fmap f . t)
    fmap f (Print c t) = Print c (fmap f t)
    fmap _ (Fail msg ) = Fail msg

instance Applicative Effect where
    pure  = Done
    (<*>) = ap

instance Monad Effect where
    return = Done
    (Done    x) >>= f = f x
    (ReadInt t) >>= f = ReadInt (t >=> f)
    (Print c t) >>= f = Print c (t >>= f)
    (Fail msg ) >>= _ = Fail msg

instance Eq a => Eq (Effect a) where
    (Done    x) == (Done    y) = x == y
    (ReadInt f) == (ReadInt g) = all (\x -> f x == g x) [minBound .. maxBound]
    (Print c t) == (Print d u) = c == d && t == u
    (Fail s   ) == (Fail t   ) = s == t
    _           == _           = False

type CGC a = StateT CGCState Effect a

data CGCState = CGCState { mem  :: IntMap Int,
                           iP   :: Int,
                           relB :: Int } deriving (Eq, Show)

readInt :: CGC Int
readInt = StateT (\s -> ReadInt (\c -> Done (c, s)))

printInt :: Int -> CGC ()
printInt c = StateT (\s -> Print c (Done ((), s)))

cgcFail :: String -> CGC a
cgcFail str = StateT (\_ -> Fail str)

data Param = Ptr Int | Val Int deriving (Show, Eq)

data Op = ADD Param Param Param
        | MUL Param Param Param
        | READ Param
        | WRITE Param
        | JZ Param Param
        | JNZ Param Param
        | LESS Param Param Param
        | CMP Param Param Param
        | RBOFF Param
        | HALT
            deriving (Eq, Show)

doOp :: Op -> CGC ()
doOp (ADD x y r ) = mathOp (+) x y r
doOp (MUL x y r ) = mathOp (*) x y r
doOp (READ  d   ) = (readInt >>= setAddr d) >> moveIP 2
doOp (WRITE m   ) = (deref m >>= printInt) >> moveIP 2
doOp (JZ  x d   ) = deref x >>= (\x' -> if x' == 0 then setIP d else moveIP 3)
doOp (JNZ x d   ) = deref x >>= (\x' -> if x' /= 0 then setIP d else moveIP 3)
doOp (LESS x y d) = do
    x' <- deref x
    y' <- deref y
    if x' < y' then setAddr d 1 else setAddr d 0
    moveIP 4
doOp (CMP x y d) = do
    x' <- deref x
    y' <- deref y
    if x' == y' then setAddr d 1 else setAddr d 0
    moveIP 4
doOp (RBOFF x) = do
    s <- get
    v <- deref x
    put $ s { relB = relB s + v }
    moveIP 2
doOp HALT = pure ()

-- Helpers
mathOp :: (Int -> Int -> Int) -> Param -> Param -> Param -> CGC ()
mathOp _ _ _ (Val _) = cgcFail "Pointer Parameter not allowed"
mathOp f x y d       = do
    x' <- deref x
    y' <- deref y
    setAddr d (x' `f` y')
    moveIP 4

deref :: Param -> CGC Int
deref (Val i   ) = pure i
deref (Ptr addr) = if addr < 0
    then cgcFail "Accessing negative mem addr"
    else IM.findWithDefault 0 addr . mem <$> get

setAddr :: Param -> Int -> CGC ()
setAddr (Val _   ) _   = cgcFail "DMA not allowed"
setAddr (Ptr addr) val = modify $ \s -> s { mem = IM.insert addr val (mem s) }

setIP :: Param -> CGC ()
setIP i = do
    i' <- deref i
    s  <- get
    put $ s { iP = i' }

moveIP :: Int -> CGC ()
moveIP i = modify $ \s -> s { iP = iP s + i }


nextInst :: CGCState -> Op
nextInst cgc =
    let opCode = fromMaybe (error "CGC Error: IP out of bounds") $ m IM.!? ip
        p0     = parsePara 0
        p1     = parsePara 1
        p2     = parsePara 2
    in  case digPad 2 opCode of
            [1, 0] -> ADD p0 p1 p2
            [2, 0] -> MUL p0 p1 p2
            [3, 0] -> READ p0
            [4, 0] -> WRITE p0
            [5, 0] -> JNZ p0 p1
            [6, 0] -> JZ p0 p1
            [7, 0] -> LESS p0 p1 p2
            [8, 0] -> CMP p0 p1 p2
            [9, 0] -> RBOFF p0
            [9, 9] -> HALT
            _      -> error $ "Invalid CGC  Instruction: " <> show opCode
  where
    parsePara n =
        let paramAddr = succ ip + n
        in  fromMaybe (error "Parameter error") $ do
                t <- last . digPad (n + 3) <$> (m IM.!? ip)
                case t of
                    0 -> Ptr <$> (m IM.!? paramAddr)
                    1 -> Val <$> (m IM.!? paramAddr)
                    2 -> Ptr . (+) (relB cgc) <$> (m IM.!? paramAddr)
                    _ -> Nothing
    ip = iP cgc
    m  = mem cgc
    digPad n int = take n $ digitsR int <> repeat 0

type Buffers = ([Int], [Int])

runCGC :: CGCState -> Buffers -> (Buffers, CGCState)
runCGC s buf = handleEff buf $ runStateT (get >>= doOp . nextInst) s
  where
    -- TODO handle empty inbuf lazily
    handleEff (i, o) (ReadInt f   ) = handleEff (tail i, o) (f $ head i)
    handleEff (i, o) (Print p eff ) = handleEff (i, p : o) eff
    handleEff _      (Fail str    ) = error $ "CGC Error: " <> str
    handleEff (i, o) (Done (_, c')) = case nextInst c' of
        HALT -> ((i, reverse o), c')
        _    -> runCGC c' (i, o)

-- API

-- Run withour input and ignore output
-- untill halt and return the final state
execCGC :: CGCState -> CGCState
execCGC s = snd $ runCGC s ([], [])

-- Run till halt and return all the output
-- in order of they were produced
evalCGC :: CGCState -> [Int] -> [Int]
evalCGC s inp = snd $ fst $ runCGC s (inp, [])

-- Run a Program
evalCGCprg :: [Int] -> [Int] -> [Int]
evalCGCprg prg inp = snd $ fst $ runCGC (initCGC prg) (inp, [])

-- Setup with memory
initCGC :: [Int] -> CGCState
initCGC m = CGCState (IM.fromList (zip [0 ..] m)) 0 0

-- Set the Verb and Noun
setVerbAndNoun :: (Int, Int) -> (CGCState -> CGCState)
setVerbAndNoun (v, n) cgc =
    cgc { mem = IM.union (IM.fromList [(1, v), (2, n)]) (mem cgc) }

-- Get the memory
readMem :: CGCState -> [Int]
readMem = fmap snd . IM.toAscList . mem
