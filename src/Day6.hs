module Day6 where

import FileParser
import Data.List.Split (splitOn)

-- left child, right sibling
type Object = String
data Tree a = T a [Tree a] | N deriving (Eq, Show)

create a = T a []
add _ N = N
add (parent, val) (T a ts) 
  | parent == a = T a ((T val []):ts)
  | otherwise   =  (T a (map (add (parent, val)) ts))


megaAdd [] [] t = t
megaAdd [] retry t = megaAdd retry [] t
megaAdd (cur@(center, obj):rest) retry t@(T a ts) =
  let t' = add cur t in
    if t' == t 
    then megaAdd rest (cur:retry) t'
    else megaAdd rest retry t'

distToRoot :: Int -> Tree a -> Int
distToRoot dist N = dist
distToRoot dist (T a ts) =
  dist + sum (map (distToRoot (dist+1)) ts)

treeify = foldr (\a tree -> add a tree) (create "COM") input
trel = foldl (\tree a -> add a tree) (create "COM") input
trel2 = foldl (flip add) (create "COM") input
-- trel3 = foldl (flip add) (create "21X")
input = [("COM", "B"), ("B", "C"), ("C", "D"), ("D", "E"), ("E", "F"), ("B", "G"), ("G", "H"), ("D", "I"), ("E", "J"), ("J", "K"), ("K", "L")]

parseOrbit s = 
  let
    os = splitOn ")" s
  in
    (os !! 0, os !! 1)

main = do
  input0 <- parseLines parseOrbit "resources/Day6.dat"
  let res = distToRoot 0 $ megaAdd input0 [] (create "COM")
  print res