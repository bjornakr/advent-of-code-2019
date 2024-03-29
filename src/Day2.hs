module Day2 where

  type OpCodePos = Int
  data Program = Program OpCodePos [Int] deriving (Eq, Show)

  replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

  getValues (Program opCodePos intcodes) =
    (intcodes !! (intcodes !! (opCodePos + 1)), intcodes !! (intcodes !! (opCodePos + 2)))

  execute op p@(Program opCodePos intcodes) =
    let
      res = uncurry op (getValues p)
    in
      Program opCodePos (replace (intcodes !! (opCodePos + 3)) res intcodes)

  step (Program opCodePos intcodes) = Program (opCodePos + 4) intcodes

  run p@(Program opCodePos intcodes) =
    case intcodes !! opCodePos of
      1 -> run $ step $ execute (+) p
      2 -> run $ step $ execute (*) p
      99 -> (head intcodes, p)

  runInit intcodes = run $ Program 0 intcodes



  realInput = [
    1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,10,19,1,19,6,23,2,23,13,27,1,
    27,5,31,2,31,10,35,1,9,35,39,1,39,9,43,2,9,43,47,1,5,47,51,2,13,
    51,55,1,55,9,59,2,6,59,63,1,63,5,67,1,10,67,71,1,71,10,75,2,75,
    13,79,2,79,13,83,1,5,83,87,1,87,6,91,2,91,13,95,1,5,95,99,1,99,2,
    103,1,103,6,0,99,2,14,0,0
    ]::[Int]

  solve1 = runInit $ replace 2 2 (replace 1 12 realInput)

  solve2 = 
    let 
      try (x, y) = runInit $ replace 2 y (replace 1 x realInput) 

      inputs = (do x <- [0..99]; y <- [0..99]; pure (x, y))

      search x (i@(noun, verb):is) = 
        case try i of
          (y, _) | x == y ->  100 * noun + verb
          _ ->                search x is

    in
      search 19690720 inputs


  -- Test

  t0 = Program 0 [1,0,0,0,99]
  t1 = Program 0 [2,3,0,3,99]
  t1op4 = Program 4 [2,3,0,3,99]
  t2 = Program 0 [2,4,4,5,99,0]
  t3 = Program 0 [1,1,1,4,99,5,6,0,99]
  t4 = Program 0 [1,0,0,3,2,3,3,8,99]
  t5 = Program 0 [1,9,10,3,2,3,11,0,99,30,40,50]
