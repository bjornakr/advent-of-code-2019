module Day5 where

type Addr = Int
type OpCodePos = Int
data Input = Input [Int] deriving (Eq, Show)
data Output = Output [Int] deriving (Eq, Show)
data Program = Program OpCodePos [Int] Input Output deriving (Eq, Show)
data ParamMode = P | I deriving (Eq, Show)
--data Op = Op [ParamMode] deriving (Eq, Show)
-- data Opcode = ADD Int Int Int | MUL Int Int Int | STR Addr | OUT Addr
data Opcode = ADD | MUL | STR | OUT | JMPT | JMPF | LTX | EQX deriving (Eq, Show)

replace pos newVal list = take pos list ++ newVal : drop (pos+1) list




-- parseOp opStr =
--   let opCode =
get :: ParamMode -> Int -> [Int] -> Int
get mode i is =
  case mode of
    P -> is !! i
    I -> i

getx :: ParamMode -> Addr -> [Int] -> Int
getx mode addr is =
  let val = is !! addr in
    case mode of
      P -> is !! val
      I -> val

-- mul :: (ParamMode, Int) -> (ParamMode, Int) -> [Int] -> Int
mul op (mx, x) (my, y) is =
  (get mx x is) `op` (get my y is)

mulx :: (Int -> Int -> Int) -> [ParamMode] -> Program -> Program
mulx op modes (Program pos is inp outp) =
  let 
    p1          = is !! (pos + 1)
    p2          = is !! (pos + 2)
    p3          = is !! (pos + 3)
    result      = mul op (modes !! 0, p1) (modes !! 1, p2) is
  in
    Program pos (replace p3 result is) inp outp
    
str (Program pos is (Input (inp:inps)) outp) =
  let
    addr = (is !! (pos+1))
    is' = replace addr inp is
    input' = (Input inps)
  in
  Program pos is' input' outp

out (Program pos is inp (Output outps)) =
  let
    addr = (is !! (pos+1))
    res = is !! addr
  in
    Program pos is inp (Output (res:outps))

jmp predicate (m0:m1:_) (Program pos is inp outp) =
  let
    p0 = getx m0 (pos+1) is
    p1 = getx m1 (pos+2) is
    pos' = if (predicate p0) then p1 else pos
  in
    Program pos' is inp outp

cond op (m0:m1:_) (Program pos is inp outp) =
  let
    p0 = getx m0 (pos+1) is
    p1 = getx m1 (pos+2) is
    p2 = is !! (pos+3)
    res = if p0 `op` p1 then 1 else 0
    is' = replace p2 res is
  in
    Program pos is' inp outp
  

parseParamMode '0' = P
parseParamMode '1' = I
    
step count (Program pos is inp outp) = Program (pos + count) is inp outp

getPos (Program pos _ _ _) = pos

evax :: Program -> Program
evax p@(Program pos is inp outp) =
  let
    opStr = reverse $ show $ is !! pos
    (op, stepCount)   = case head opStr of
              '1' -> (ADD, 4)
              '2' -> (MUL, 4)
              '3' -> (STR, 2)
              '4' -> (OUT, 2)
              '5' -> (JMPT, 3)
              '6' -> (JMPF, 3)
              '7' -> (LTX, 4)
              '8' -> (EQX, 4)
    modes = (map parseParamMode (drop 2 opStr)) ++ [P,P,P,P] --(repeat P)
    res :: Program
    res = case op of
            ADD -> mulx (+) modes p
            MUL -> mulx (*) modes p
            STR -> str p
            OUT -> out p
            JMPT -> jmp (\i -> i > 0) modes p
            JMPF -> jmp ((==) 0) modes p
            LTX -> cond (<) modes p
            EQX -> cond (==) modes p
    posHasChanged = (getPos res) /= pos 

  in
    if posHasChanged then res else step stepCount res
    

run p@(Program opCodePos intcodes inp outp) =
  case intcodes !! opCodePos of
    99 -> (opCodePos, p)
    _  -> run $ evax p

runInit intcodes = run $ Program 0 intcodes (Input []) (Output []) 


t0 = [1002,4,3,4,33]
t1 = [3,0,4,0,99]
p1 = Program 0 t1 (Input [9]) (Output []) 


t99 = [3,225,1,225,6,6,1100,1,238,225,104,0,1102,91,92,225,1102,85,13,225,1,47,17,224,101,-176,224,224,4,224,1002,223,8,223,1001,224,7,224,1,223,224,223,1102,79,43,225,1102,91,79,225,1101,94,61,225,1002,99,42,224,1001,224,-1890,224,4,224,1002,223,8,223,1001,224,6,224,1,224,223,223,102,77,52,224,1001,224,-4697,224,4,224,102,8,223,223,1001,224,7,224,1,224,223,223,1101,45,47,225,1001,43,93,224,1001,224,-172,224,4,224,102,8,223,223,1001,224,1,224,1,224,223,223,1102,53,88,225,1101,64,75,225,2,14,129,224,101,-5888,224,224,4,224,102,8,223,223,101,6,224,224,1,223,224,223,101,60,126,224,101,-148,224,224,4,224,1002,223,8,223,1001,224,2,224,1,224,223,223,1102,82,56,224,1001,224,-4592,224,4,224,1002,223,8,223,101,4,224,224,1,224,223,223,1101,22,82,224,1001,224,-104,224,4,224,1002,223,8,223,101,4,224,224,1,223,224,223,4,223,99,0,0,0,677,0,0,0,0,0,0,0,0,0,0,0,1105,0,99999,1105,227,247,1105,1,99999,1005,227,99999,1005,0,256,1105,1,99999,1106,227,99999,1106,0,265,1105,1,99999,1006,0,99999,1006,227,274,1105,1,99999,1105,1,280,1105,1,99999,1,225,225,225,1101,294,0,0,105,1,0,1105,1,99999,1106,0,300,1105,1,99999,1,225,225,225,1101,314,0,0,106,0,0,1105,1,99999,8,226,677,224,102,2,223,223,1005,224,329,1001,223,1,223,1007,226,226,224,1002,223,2,223,1006,224,344,101,1,223,223,108,226,226,224,1002,223,2,223,1006,224,359,1001,223,1,223,107,226,677,224,102,2,223,223,1006,224,374,101,1,223,223,8,677,677,224,102,2,223,223,1006,224,389,1001,223,1,223,1008,226,677,224,1002,223,2,223,1006,224,404,101,1,223,223,7,677,677,224,1002,223,2,223,1005,224,419,101,1,223,223,1108,226,677,224,1002,223,2,223,1005,224,434,101,1,223,223,1108,226,226,224,102,2,223,223,1005,224,449,1001,223,1,223,107,226,226,224,102,2,223,223,1005,224,464,101,1,223,223,1007,677,677,224,102,2,223,223,1006,224,479,101,1,223,223,1007,226,677,224,102,2,223,223,1005,224,494,1001,223,1,223,1008,226,226,224,1002,223,2,223,1005,224,509,1001,223,1,223,1108,677,226,224,1002,223,2,223,1006,224,524,1001,223,1,223,108,677,677,224,1002,223,2,223,1005,224,539,101,1,223,223,108,226,677,224,1002,223,2,223,1005,224,554,101,1,223,223,1008,677,677,224,1002,223,2,223,1006,224,569,1001,223,1,223,1107,677,677,224,102,2,223,223,1005,224,584,1001,223,1,223,7,677,226,224,102,2,223,223,1005,224,599,1001,223,1,223,8,677,226,224,1002,223,2,223,1005,224,614,1001,223,1,223,7,226,677,224,1002,223,2,223,1006,224,629,101,1,223,223,1107,677,226,224,1002,223,2,223,1005,224,644,1001,223,1,223,1107,226,677,224,102,2,223,223,1006,224,659,1001,223,1,223,107,677,677,224,1002,223,2,223,1005,224,674,101,1,223,223,4,223,99,226]
p99 = Program 0 t99 (Input [1]) (Output [])
-- eval (Op (m:modes)) (Program _ i:is) =

t201 = [3,9,8,9,10,9,4,9,99,-1,8]
p201 = Program 0 t201 (Input [8]) (Output [])

t202 = [3,9,7,9,10,9,4,9,99,-1,8]
p202 = Program 0 t201 (Input [8]) (Output [])

t209 = [3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99]
p209 = Program 0 t209 (Input [8]) (Output [])


p299 = Program 0 t99 (Input [5]) (Output [])
-- 1002,4,3,4,33

-- MUL 
--       0 1 2 3  4 5 6 7 8
test0 = [5,4,5,99,1,6,4,8,99]
-- JMP to address 6
-- OUT 99

ptest0 = Program 0 test0 (Input []) (Output [])