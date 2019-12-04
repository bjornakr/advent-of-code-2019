
module Day3 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, sortOn)
import FileParser

data Direction = L | R | U | D deriving (Eq, Show)
data Destination = Destination Direction Int deriving (Eq, Show)

-- data Direction = L Int | R Int | U Int|  D Int deriving (Eq, Show)

type Coord = (Int, Int)

-- toCoords direction (x, y) =
--   case direction of
--     L i -> (x-i, y)
--     R i -> (x+i, y)
--     U i -> (x, y-i)
--     D i -> (x, y+i)

-- goTowards direction from@(x, y) destination travelled =
--   if from == destination then travelled
--   else
--     let next = 
--           case direction of
--             L _ -> (x-1, y)
--             R _ -> (x+1, y)
--             U _ -> (x, y-1)
--             D _ -> (x, y+1)
--     in goTowards direction next destination (next:travelled)

goTowards2 :: Destination -> Coord -> [Coord] -> [Coord]
goTowards2 (Destination _ 0) _ travelled = travelled
goTowards2 (Destination direction distance) cur@(x, y) travelled =
  let next =
        case direction of
          L -> (x-1, y)
          R -> (x+1, y)
          U -> (x, y-1)
          D -> (x, y+1)
  in goTowards2 (Destination direction (distance - 1)) next (next:travelled)


toCoordsSet :: [Destination] -> Set Coord
toCoordsSet destinations =
  let
    loop [] acc = acc
    loop (d:destinations) (a:acc) =
      loop destinations (goTowards2 d a [] ++ acc)  -- ((goTowards d a (toCoords d a) []) ++ acc)

      --(toCoords d a:a:acc)
  in
    Set.fromList $ loop destinations [(0,0)]

toCoordsSets (ds1, ds2) = (toCoordsSet ds1, toCoordsSet ds2)

findClosest :: Set Coord -> (Coord, Int)
findClosest intersections =
  let
    coordSum (x, y) = abs x + abs y
    sums = Set.map (\c -> (c, coordSum c)) intersections
  in
    head $ sortOn snd $ Set.toList sums


solve1 (cable1, cable2) = findClosest $ uncurry Set.intersection (toCoordsSets (cable1, cable2))

parseDirection :: Char -> Direction
parseDirection c = case c of
  'L' -> L
  'R' -> R
  'U' -> U
  'D' -> D

parse destinationString@(direction:distance) =
  Destination (parseDirection direction) (read distance)


main = do
  putStrLn "=== DAY 3 - PART 1 ==="
  input <- parseCsvLines parse "resources/Day3.dat"
  let res = solve1 (input !! 0, input !! 1)
  print res
  


-- TEST

t0 = (
  map parse ["R8","U5","L5","D3"],
  map parse ["U7","R6","D4","L4"]
  )

t1 = (
  map parse ["R75","D30","R83","U83","L12","D49","R71","U7","L72"],
  map parse ["U62","R66","U55","R34","D71","R55","D58","R83"]
  )
t1f = fst t1
