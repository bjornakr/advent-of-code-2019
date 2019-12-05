
module Day3 where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, sortOn)
import FileParser

type Distance = Int
data Direction = L | R | U | D deriving (Eq, Show)
data Destination = Destination Direction Distance deriving (Eq, Show)

-- data Direction = L Int | R Int | U Int|  D Int deriving (Eq, Show)

type Steps = Int
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
goTowards2 (Destination direction distance) (x, y) travelled =
  let next =
        case direction of
          L -> (x-1, y)
          R -> (x+1, y)
          U -> (x, y-1)
          D -> (x, y+1)
  in goTowards2 (Destination direction (distance - 1)) next (next:travelled)


nextCoordinates direction (x, y) =
  case direction of
    L -> (x-1, y)
    R -> (x+1, y)
    U -> (x, y-1)
    D -> (x, y+1)

-- todo refactor
--goTowards3 ((Destination direction 0):ds) cur intersections steps
goTowards3 ((Destination direction distance):ds) cur@(x, y) intersections steps
  | cur `elem` intersections = (steps, cur)
  | distance == 0 = goTowards3 ds cur intersections steps
  | otherwise =
    let next =
          case direction of
            L -> (x-1, y)
            R -> (x+1, y)
            U -> (x, y-1)
            D -> (x, y+1)
    in goTowards3 (Destination direction (distance - 1):ds) next intersections (steps+1)

goTowards4 :: [Destination] -> Coord -> [Coord] -> Int -> [(Coord, Int)] -> [(Coord, Int)]
goTowards4 [] _ _ _ costeps = costeps
goTowards4 ((Destination direction distance):ds) cur@(x, y) intersections steps costeps =
  let
    destination' = if distance == 0 then ds else (Destination direction (distance-1)):ds
    coord' = (nextCoordinates direction cur)
    costeps' = if (cur `elem` intersections) then ((cur, steps):costeps) else costeps
  in
    goTowards4 destination' coord' intersections (steps+1) costeps'

goTowards5 [] _ _ steps costeps = (steps, costeps)
-- goTowards5 [] _ _ steps costeps = costeps
goTowards5 ((Destination direction distance):ds) cur@(x, y) intersections steps costeps =
  let
    destination' = if distance == 0 then ds else (Destination direction (distance-1)):ds
    coord' = if distance == 0 then cur else (nextCoordinates direction cur)
    costeps' = if (cur `elem` intersections) then ((cur, length steps):costeps) else costeps
  in
    goTowards5 destination' coord' intersections (cur:steps) costeps'


goTowards6 [] _ _ steps costeps = (steps, costeps)
-- goTowards5 [] _ _ steps costeps = costeps
goTowards6 ((Destination direction 0):ds) cur@(x, y) intersections steps costeps =
  goTowards6 ds cur intersections steps costeps
goTowards6 ((Destination direction distance):ds) cur@(x, y) intersections steps costeps =
  let
    costeps' = if (cur `elem` intersections) then ((cur, length steps):costeps) else costeps
    coord' = if distance == 0 then cur else (nextCoordinates direction cur)
  in
    goTowards6 ((Destination direction (distance-1)):ds) coord' intersections (cur:steps) costeps'



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

-- solve2 cable1 cable2 =
--   let
--     intersections = uncurry Set.intersection (toCoordsSets (cable1, cable2))
--   in
--     sum $ map (\ds -> goTowards3 ds (0, 0) intersections 0) [cable1, cable2]

solve3 cable1 cable2 =
  let
    intersections = uncurry Set.intersection (toCoordsSets (cable1, cable2))
    (s11, goal1) = goTowards3 cable1 (0, 0) intersections 0
    (s21, _) = goTowards3 cable2 (0, 0) [goal1] 0
    (s12, goal2) = goTowards3 cable2 (0, 0) intersections 0
    (s22, _) = goTowards3 cable1 (0, 0) [goal2] 0
  in
    min (s11+s21) (s12+s22)

solve4 (cable1, cable2) =
  let
    intersections = uncurry Set.intersection (toCoordsSets (cable1, cable2))
    c1 = snd $ goTowards6 cable1 (0,0) intersections [] []
    c2 = snd $ goTowards6 cable2 (0,0) intersections [] []
    cs1 = map snd $ sortOn fst c1
    cs2 = map snd $ sortOn fst c2
  in
    --(cs1, cs2)
    head $ sort $ zipWith (+) cs1 cs2

    


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
  cable1:cable2:[] <- parseCsvLines parse "resources/Day3.dat"
  let res = solve1 (cable1, cable2)
  print res

  putStrLn "=== DAY 3 - PART 2 ==="
  let res2 = solve4 (cable1, cable2)
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
gtw5 = goTowards5 (fst t0) (0,0) (Set.toList t0isect) [] []


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
