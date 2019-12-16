module Day8 where

import FileParser
import Data.List.Split

type Layer = [Int]



z0 width height = chunksOf height . chunksOf width
z1 :: [[String]] -> [[Int]]
z1 = map (\x -> map (\y -> read y ) x)

t0 = z0 3 2 "123456789012"

s = "123456789012"
parse :: Int -> Int -> String -> [[Int]]
parse width height =
  let 
    intify = map (\x -> map (\y -> read y ) x)
  in
    intify . chunksOf height . chunksOf width
-- z0 width height str = ((take width) str, drop width str)

-- z1 _ 0 _ = []
-- z1 w h s acc =
--   let
--     (cur, s') = z0 w h s
--   in
--     z1 w h s' 
-- parseLayer :: Int -> Int -> String -> (Layer, String)
-- parseLayer width height str =
--   let 
--     loop :: (Layer, String) -> (Layer, String)
--     loop (acc, strx)
--       | length acc == height = (acc, strx)
--       | otherwise = 
--           let
--             ls = take width strx
--             nextPixel = read ls
--           in
--             loop (nextPixel:acc, drop width strx)
--   in
--     loop ([], str)

-- -- parse width height str =
-- --   foldr (\(layers, str') -> 
-- --     let
-- --       (l, s) = (parseLayer width height str')
-- --     in
-- --       (l:layers, s)
-- --   ) [] ([], str)
--   --(parseLayer width height str) : (parse width height )
-- parse width height str =

-- main =
--   parseLines 