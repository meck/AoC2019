module Day6 (day06a, day06b) where
import           Util (splitOn)
import Data.Tree
import           Data.Maybe                     ( catMaybes
                                                , isJust
                                                )


makeTree :: [(String, String)] -> Tree String
makeTree inp = unfoldTree getNodes getRoot
  where getRoot = head $ filter ((== "COM") . fst) inp
        getNodes (_, y) = if null bla then (y, []) else (y, bla)
            where bla = filter ((==y). fst) inp

solveA :: (Num a, Enum a) => Tree b -> a
-- solveA as = sum $ concatMap (\(i,bs) -> fmap (const i) bs ) $ zip [1 .. ] $ levels as
solveA as = sum $ concatMap (\(i,bs) -> fmap (const i) bs ) $ zip [1 .. ] $ levels as

day06a :: String -> String
day06a i = show $ solveA $ makeTree $ fmap tail . break (==')') <$> lines i


-- distToRoot :: (Num p, Ord p, Enum p, Eq b) => Tree b -> b -> p
-- distToRoot (Node l _) x | l == x = True
-- distToRoot (Node _ []) _ = False
-- distToRoot (Node _ xs) x = maximum $ map (succ . flip distToRoot x) xs



day06b :: String -> String
day06b i = show $ pred $ pred  $  getDistance ( makeTree $ fmap tail . break (==')') <$> lines i) "YOU" "SAN"



testdata=  makeTree $ fmap tail . break (==')') <$>  lines "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\n"
-- testTree =  makeTree $ (fmap tail . break (==')')) <$> lines testdata2


-- testdata2="COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN\n"

-- test2 = foldTree go test
--   where go "COM" res = sum res
--         go tr res =  
-- day06b = id
dist' :: (Eq a) => a -> Tree a -> Maybe Int
dist' x = foldTree go
  where go l _ | l == x = Just 0
        go _ [] = Nothing
        go _ xs = bla $ fmap succ <$> xs

bla :: [Maybe Int] -> Maybe Int
bla xs = (\x -> if null x then Nothing else Just (minimum x) ) $ catMaybes xs


-- cman :: (Eq a) => Tree a -> a -> a -> Maybe (Tree a)
-- cman n@(Node l xs) x y | l== x && y `elem` xs = Just n

-- go' xs = sum $ catMaybes xs
-- dista' (Node l _ ) x = Just 1
-- dista' (_ [] ) _  = Nothing
-- dista' (_ xs ) x  = 


-- | Find the distance between two leaves in a tree.
getDistance :: (Eq a) => Tree a -> a -> a -> Int
getDistance (Node l [] ) x y = boolToInt $ l `elem` [x, y]
getDistance n@(Node _ xs ) x y
    | not (elem x ls || elem y ls)      = 0
    | otherwise = sum . (:) (boolToInt notShared) . map (\t -> getDistance t x y) $ xs
  where
    -- Only count nodes that have one or the other, not shared or empty
    ls = flatten n
    notShared =  not (elem x ls && elem y ls)

-- | Convert a bool to an integer
boolToInt :: Bool -> Int
boolToInt True  = 1
boolToInt False = 0
