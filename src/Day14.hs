module Day14 (day14a, day14b) where

import           Data.Bifunctor                 ( second )
import           Data.Char                      ( isUpper )
import           Data.Map.Lazy                  ( Map )
import qualified Data.Map.Lazy                 as M
import           Parsing
import           Text.ParserCombinators.ReadP
import           Util                           ( firstEq )

type Chem = String
type Ingredients = Map Chem Int
type Rules = Map Chem (Int, [(Chem,Int)])

solveForFuel :: Int -> Rules -> Int
solveForFuel i r = go (M.singleton "FUEL" i) M.! "ORE"
    where go = firstEq . iterate (stepIng r)

stepIng :: Rules -> Ingredients -> Ingredients
stepIng rul req = foldr (M.unionWith (+) . (M.fromList . step))
                        M.empty
                        (M.toList req)
  where
    step (ch, am) | Just (o, inp) <- rul M.!? ch, am > 0 =
        let (times, rest) = am `divMod'` o
            base          = second (* times) <$> inp
        in  (ch, -rest) : base
    step (_ , 0 ) = []
    step (ch, am) = [(ch, am)]

divMod' :: Integral b => b -> b -> (b, b)
divMod' n d | r > 0     = (q + 1, d - r)
            | otherwise = (q, 0)
    where (q, r) = n `divMod` d

search :: Integral a => a -> a -> (a -> Ordering) -> Either (a, a) a
search i j _ | j < i = Left (i, j)
search i j f         = case f k of
    LT -> search i (k - 1) f
    EQ -> Right k
    GT -> search (k + 1) j f
    where k = (i + j) `div` 2

ingr :: ReadP (Chem, Int)
ingr = do
    n <- integer
    skipSpaces
    ch <- many1 (satisfy isUpper)
    pure (ch, n)

rules :: ReadP Rules
rules = fmap M.fromList $ flip sepBy skipSpaces $ do
    inputs <- commaSepListOf ingr
    skipSpaces *> string "=>" *> skipSpaces
    outp <- ingr
    pure (fst outp, (snd outp, inputs))

day14a :: String -> String
day14a = show . solveForFuel 1 . run rules

day14b :: String -> String
day14b inp = show $ either snd id s
  where
    s = search 1 t (compare t . flip solveForFuel r)
    r = run rules inp
    t = 1000000000000
