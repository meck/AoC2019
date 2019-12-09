module Day6 (day06a, day06b) where
import           Data.List                      ( sortOn )
import           Data.Maybe                     ( fromMaybe
                                                , listToMaybe
                                                )
import           Data.Tree

solveA :: Tree b -> Int
solveA = sum . concatMap (uncurry (<$)) . zip [0 ..] . levels

path :: (Eq a) => Tree a -> a -> [a]
path (Node l _) x | l == x = [l]
path (Node l sf) x         = l : path'
  where
    path' =
        fromMaybe []
            $   listToMaybe
            $   sortOn length
            $   flip path x
            <$> filter (elem x) sf

getDist :: (Eq a) => Tree a -> a -> a -> Int
getDist t x y = length (drop sharedLen xp) + length (drop sharedLen yp)
  where
    xp        = path t x
    yp        = path t y
    sharedLen = length $ takeWhile id $ zipWith (==) xp yp

makeTree :: [(String, String)] -> Tree String
makeTree inp = root "COM"
  where
    root r = Node r (root <$> nodes r)
    nodes r = snd <$> filter ((== r) . fst) inp

parse :: String -> [(String, String)]
parse i = fmap tail . break (== ')') <$> lines i

day06a :: String -> String
day06a = show . solveA . makeTree . parse

day06b :: String -> String
day06b i = show $ subtract 2 $ getDist (makeTree $ parse i) "YOU" "SAN"
