
module Day3 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, sortOn)
import FileParser

type Distance = Int
data Direction = L | R | U | D deriving (Eq, Show)
data Destination = Destination Direction Distance deriving (Eq, Show)
type Steps = Int
type Coord = (Int, Int)



nextCoordinates direction (x, y) =
  case direction of
    L -> (x-1, y)
    R -> (x+1, y)
    U -> (x, y-1)
    D -> (x, y+1)

hasArrived (Destination _ distance) = distance == 0

moveOneTowards (Destination direction distance) =  Destination direction (distance - 1)


goTowards2 :: Destination -> Coord -> [Coord] -> [Coord]
goTowards2 (Destination _ 0) _ travelled = travelled
goTowards2 (Destination direction distance) curCoord@(x, y) travelled =
  let coord' = nextCoordinates direction curCoord
  in goTowards2 (Destination direction (distance - 1)) coord' (coord':travelled)

countSteps destinations intersections =
  let
    loop [] _ _ costeps = costeps
    loop (destination@(Destination direction distance):ds) cur@(x, y) steps costeps
      | hasArrived destination = loop ds cur steps costeps
      | otherwise = 
          let
            costeps' = if (cur `elem` intersections) then ((cur, steps):costeps) else costeps
            coord' = (nextCoordinates direction cur)
          in
            loop ((moveOneTowards destination):ds) coord' (steps+1) costeps'
  in
    loop destinations (0, 0) 0 []



toCoordsSet :: [Destination] -> Set Coord
toCoordsSet destinations =
  let
    loop [] acc = acc
    loop (d:destinations) (a:acc) =
      loop destinations (goTowards2 d a [] ++ acc)
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

solve2 (cable1, cable2) =
  let
    intersections = uncurry Set.intersection (toCoordsSets (cable1, cable2))
    costeps c = countSteps c intersections
    costepsSorted s = map snd $ sortOn fst s
    c1 = countSteps cable1 intersections
    c2 = countSteps cable2 intersections
    cs1 = map snd $ sortOn fst c1
    cs2 = map snd $ sortOn fst c2
  in
    head $ sort $ zipWith (+) cs1 cs2


parseDirection :: Char -> Direction
parseDirection c = case c of
  'L' -> L
  'R' -> R
  'U' -> U
  'D' -> D

parse destinationString@(direction:distance) =
  Destination (parseDirection direction) (read distance)





main1 = do
  putStrLn "=== DAY 3 - PART 1 ==="
  cable1:cable2:[] <- parseCsvLines parse "resources/Day3.dat"
  let res = solve1 (cable1, cable2)
  print res

main2 = do
  cable1:cable2:[] <- parseCsvLines parse "resources/Day3.dat"

  putStrLn "=== DAY 3 - PART 2 ==="
  let res2 = solve2 (cable1, cable2)
  print res2

-- TEST

t0 = (
  map parse ["R8","U5","L5","D3"],
  map parse ["U7","R6","D4","L4"]
  )

t1 = (
  map parse ["R75","D30","R83","U83","L12","D49","R71","U7","L72"],
  map parse ["U62","R66","U55","R34","D71","R55","D58","R83"]
  )

t2 = (
  map parse ["R98","U47","R26","D63","R33","U87","L62","D20","R33","U53","R51"],
  map parse ["U98","R91","D20","R16","D67","R40","U7","R15","U6","R7"]
  )

t1f = fst t1


t0isect = uncurry Set.intersection (toCoordsSets t0)


-- ...........
-- .+-----+...
-- .|.....|...
-- .|..+--X-+.
-- .|..|..|.|.
-- .|.-X--+.|.
-- .|..|....|.
-- .|.......|.
-- .o-------+.
-- ...........
