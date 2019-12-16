module Day8 where

import FileParser
import Data.List
import Data.List.Split

type Layer = [String]



z0 width height = chunksOf height . chunksOf width
z1 :: [[String]] -> [[Int]]
z1 = map (map read)

t0 = z0 3 2 "123456789012"

s = "123456789012"
parse :: Int -> Int -> String -> [[String]]
parse width height = chunksOf height . chunksOf width

noOfDigits :: Char -> [String] -> Int
noOfDigits digit layer =
  let
    c = length . filter (digit ==)
  in
    sum $ map c layer


calcx :: [Layer] -> [(Int, Layer)]
calcx = map (\l -> (noOfDigits '0' l, l))
  --foldr (\layer acc -> (noOfDigits '0' layer, layer):acc) [] layers

merge '0' _ = '0'
merge '1' _ = '1'
merge '2' new = new


renderRow :: String -> String -> String
renderRow = zipWith merge 

-- renderLayer old new =
--   map (\(o, n) -> renderRow o n) (zip old new)

-- renderx :: [Layer] -> Layer
-- renderx (first:rest) =
--   foldr (\newLayer last -> renderLayer last newLayer) first rest

rendery :: [String] -> String
rendery (l:[]) = l
rendery (l1:l2:rest) =
  let new = renderRow l1 l2 in
    rendery (new : rest)


main = do
  let (width, height) = (25, 6)
  r0 <- parseLines (parse width height) "resources/Day8.dat"
  let layers = head r0
  let r0@(zeros, layer) = minimum $ calcx layers
  let r1@(ones, twos) = (noOfDigits '1' layer, noOfDigits '2' layer)
  --print r0
  --print r1
  let renderAll = map rendery (transpose layers)
  print renderAll
  print "Done!"

test = do
  let a = parse 2 2 "0222112222120000"
  let b = transpose a
  let r = map rendery b
  (b, r)

