module Day6 where

import FileParser
import Data.List.Split (splitOn)
import Debug.Trace
import qualified Data.Map.Strict as Map
import Data.List (sort)
import Data.Maybe
import qualified Data.Set as Set
-- left child, right sibling


type Object = String


data Tree a = T a [Tree a] | N deriving (Eq, Show)

create a = T a []
add _ N = N
add (parent, val) (T a ts)
  | parent == a = T a (T val []:ts)
  | otherwise   =  T a (map (add (parent, val)) ts)


megaAdd [] [] _ t = t
megaAdd [] retry lr t
  | retry == lr = t
  | otherwise = trace ("length retry" ++ (show $ length retry)) (megaAdd retry [] retry t)
megaAdd (cur@(center, obj):rest) retry lr t@(T a ts) =
  let t' = add cur t in
    if t' == t
    then megaAdd rest (cur:retry) lr t'
    else megaAdd rest retry lr t'


-- swibbleAdd [] map = map
-- swibbleAdd ((c, obj):os) map =
swobbleMap :: [(String, [String])] -> Map.Map String [String]
swobbleMap = Map.fromListWith (++)

listify = map (\(a,b) -> (a,[b]))

mappifyInput = swobbleMap . listify

sumDepth :: Int -> Map.Map String [String] -> String -> Int
sumDepth depth m cur =
    case Map.lookup cur m of
      Nothing -> depth
      Just children -> depth + sum (Prelude.map (sumDepth (depth+1) m) children)



distToRoot :: Int -> Tree a -> Int
distToRoot dist N = dist
distToRoot dist (T a ts) =
  dist + sum (map (distToRoot (dist+1)) ts)

treeify = foldr (\a tree -> add a tree) (create "COM") input
trel = foldl (\tree a -> add a tree) (create "COM") input
trel2 = foldl (flip add) (create "COM") input
-- trel3 = foldl (flip add) (create "21X")
input = [("COM", "B"), ("B", "C"), ("C", "D"), ("D", "E"), ("E", "F"), ("B", "G"), ("G", "H"), ("D", "I"), ("E", "J"), ("J", "K"), ("K", "L")]

pathTo :: String -> Map.Map String [String] -> [String] -> String -> Maybe [String]
pathTo dest m steps cur
  | cur == dest = Just $ cur:steps
  | otherwise =
    case Map.lookup cur m of
      Nothing -> Nothing
      Just children -> 
        headOpt $ catMaybes $ map (pathTo dest m (cur:steps)) children


headOpt :: [a] -> Maybe a
headOpt [] = Nothing 
headOpt (a:_) = Just a

--shortestPath :: String -> String -> String -> Map.Map String [String] -> Maybe Int
shortestPath cur dest1 dest2 m =
  do
    a <- pathTo dest1 m [] cur
    b <- pathTo dest2 m [] cur    
    let sa = Set.fromList a
    let sb = Set.fromList b
    let c = Set.intersection sa sb
    pure $ (length c)




parseOrbit s =
  let
    os = splitOn ")" s
  in
    (os !! 0, os !! 1)


main = do
  input0 <- parseLines parseOrbit "resources/Day6.dat"
  -- let res = distToRoot 0 $ megaAdd input0 [] [] (create "COM")
  let res = sumDepth 0 (mappifyInput input0) "COM"
  print res


main2 = do
  input2 <- parseLines parseOrbit "resources/Day6.dat"
  let res = shortestPath "COM" "SAN" "YOU" (mappifyInput input2)
  print res

testt :: Int -> Int
testt x = trace ("this is test: " ++ (show x)) $ 3 + x

t0 = megaAdd input [] [] (create "COM")
minput = mappifyInput input
