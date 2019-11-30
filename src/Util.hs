module Util (strip, linesStrip, Data.List.Split.splitOn, mostFreq) where

import           Data.List.Split                ( splitOn )
import qualified Data.Text                     as T
import           Data.List                      ( sort
                                                , group
                                                , maximumBy
                                                )
import           Data.Ord                       ( comparing )

strip :: String -> String
strip = T.unpack . T.strip . T.pack

linesStrip :: String -> [String]
linesStrip = fmap (T.unpack . T.strip) . T.lines . T.strip . T.pack

mostFreq :: Ord a => [a] -> [a]
mostFreq = maximumBy (comparing length) . group . sort
