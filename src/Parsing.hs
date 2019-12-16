module Parsing (integer, integerAnd, run, commaSepListOf) where

import           Data.Char
import           Data.List                      ( minimumBy )
import           Data.Ord                       ( comparing )
import           Text.ParserCombinators.ReadP

integer :: ReadP Int
integer =
    skipSpaces
        >>  read
        <$> ((++) <$> option "" (string "-") <*> many1 (satisfy isDigit))

integerAnd :: ReadP a -> ReadP Int
integerAnd r = integer >>= \n -> r >> return n

commaSepListOf :: ReadP a -> ReadP [a]
commaSepListOf a = do
    x  <- a
    xs <- many (char ',' >> a)
    return (x : xs)

run :: ReadP a -> String -> a
run parser s
    | null rest = a
    | otherwise = error $ "AOC Incomplete parse, remaining: \"" <> rest <> "\""
    where (a, rest) = minimumBy (comparing snd) $ readP_to_S (parser <* skipSpaces) s
