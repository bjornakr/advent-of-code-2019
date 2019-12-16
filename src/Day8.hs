module Day8 where

import FileParser
import Data.List.Split

type Layer = [Int]



z0 width height = chunksOf height . chunksOf width
z1 :: [[String]] -> [[Int]]
z1 = map (\x -> map (\y -> read y ) x)

t0 = z0 3 2 "123456789012"

s = "123456789012"
parse :: Int -> Int -> String -> [[String]]
parse width height = chunksOf height . chunksOf width

noOfZeroDigits pixels =
  let 
    c = length $ filter ((==) '0')
  in
    map 

main = do
  let (width, height) = (3,2)
  parseLines (parse width height) "resources/Day8.dat"
