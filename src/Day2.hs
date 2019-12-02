module Day2 where

  data Program = Program Int [Int] deriving (Eq, Show)

  replace pos newVal list = take pos list ++ newVal : drop (pos+1) list

  getValues (Program opCodePos intcodes) =
    (intcodes !! (intcodes !! (opCodePos + 1)), intcodes !! (intcodes !! (opCodePos + 2)))

  execMath p@(Program opCodePos intcodes) op =
    let
      vs = getValues p
      res = uncurry op vs
    in
      Program opCodePos (replace (opCodePos + 3) res intcodes)

  step (Program opCodePos intcodes) = Program (opCodePos + 4) intcodes

  run p@(Program opCodePos intcodes) =
    case intcodes !! opCodePos of
      1 -> run $ step $ execMath p (+)
      2 -> run $ step $ execMath p (-)
      99 -> (head intcodes, p)

  runInit intcodes = run $ Program 0 intcodes
