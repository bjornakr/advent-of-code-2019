module Day7 where 

import qualified IntCodeCpu as Cpu
import Data.List (sortBy, sort, permutations)
import Debug.Trace
allPhaseSettings :: [[Int]]
allPhaseSettings = permutations [0..4]

-- allPhaseSettings = do
--   a <- validPhaseSettings
--   b <- validPhaseSettings
--   c <- validPhaseSettings
--   d <- validPhaseSettings
--   e <- validPhaseSettings
--   pure [a,b,c,d,e]

  
runPhase p prevOut =
  let 
    output = Cpu.getOutput $ Cpu.run $ Cpu.progInit y (Cpu.Input [p, prevOut])
  in
    head output

runPhases [] out aps  = (out, aps)
runPhases (p:ps) prevOut aps =
  runPhases ps (runPhase p prevOut) aps

runAllPhases = map (\pSettings -> runPhases pSettings 0 pSettings)
-- solve1 = head $ reverse $ sort $ 
solve1 = head $ reverse $ sort $ runAllPhases allPhaseSettings

test = [[4,3,2,1,1]]
test2 = [[0,0,0,0,0], [0,1,2,3,4], [1,0,4,3,2]]
ti0 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
ti1 = [3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0]
ti2 = [3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0]
i0 = [3,8,1001,8,10,8,105,1,0,0,21,46,59,84,93,102,183,264,345,426,99999,3,9,1002,9,4,9,1001,9,3,9,102,2,9,9,1001,9,5,9,102,3,9,9,4,9,99,3,9,1002,9,3,9,101,4,9,9,4,9,99,3,9,1002,9,4,9,1001,9,4,9,102,2,9,9,1001,9,2,9,1002,9,3,9,4,9,99,3,9,1001,9,5,9,4,9,99,3,9,1002,9,4,9,4,9,99,3,9,101,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,1002,9,2,9,4,9,99,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,101,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,99,3,9,1002,9,2,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,99,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,3,9,1001,9,1,9,4,9,3,9,101,1,9,9,4,9,3,9,102,2,9,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1001,9,1,9,4,9,99,3,9,101,1,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,102,2,9,9,4,9,3,9,1002,9,2,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,3,9,1001,9,2,9,4,9,3,9,101,2,9,9,4,9,3,9,101,1,9,9,4,9,3,9,101,1,9,9,4,9,99]


--Cpu.progInit y (Cpu.input [])
y = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
py1 = [[9,8,7,6,5]]
solve2 = reverse $ sort $ runAllPhases py1

type PhaseSetting = Int
type InputUnit = Int
type OutputUnit = Int
-- type LoadedAmp = InputUnit -> Cpu.Program -> Cpu.Program

-- runAmplifier :: PhaseSetting -> InputUnit -> Cpu.Program -> Cpu.Program
-- runAmplifier ps x prog =
--   Cpu.run $ Cpu.addInput prog


type AmpState = Cpu.Program

runAmplifier :: InputUnit -> AmpState -> (OutputUnit, AmpState)
runAmplifier iu prog0 =
  let
    prog1 = Cpu.addInput iu prog0
    prog2 = Cpu.run $ prog1 --trace ("Running: " ++ (show prog1)) prog1
    (output, prog3) = Cpu.pop prog2
  in
    (output, prog3) --trace (show (output, prog3)) (output, prog3)

runAmps :: [AmpState] -> InputUnit -> [AmpState] -> (OutputUnit, [AmpState])
runAmps [] iu as' = (iu, reverse as')
runAmps (ampState:as) iu as' =
  let
    (output, ampState') = runAmplifier iu ampState
  in
    runAmps as output (ampState' : as')

finalizeAmps :: [AmpState] -> InputUnit -> (OutputUnit, AmpState)
finalizeAmps amps iu =
  let
    (output, amps') = runAmps amps iu []
    final = last amps'
  in
    case final of
      p@(Cpu.Terminated _ _ _) -> (output, p)
      _ -> finalizeAmps amps' output

-- runAmps :: [LoadedAmp] -> InputUnit -> Cpu.Program -> [Cpu.Program] -> Cpu.Program
-- runAmps (amp:amps) iu prog executedProgs = 
--   let
--     prog' = amp iu prog
--     (output, prog'') = Cpu.pop $ prog'
--   in
--     runAmps amps output prog (prog'' : executedProgs)

loadAmp amp phaseSetting =
  Cpu.addInput phaseSetting amp

runPhasex prog phaseSettings = 
  let
    amps = map (loadAmp prog) phaseSettings
  in
    finalizeAmps amps 0

psy = permutations [5..9]


main prog =
  let
    res = map (runPhasex prog) psy
    outputs = map fst res
  in
    head $ reverse $ sort $ outputs

yt0 = [3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0]
pyt0 = Cpu.progInit yt0 (Cpu.Input [])


yt1 = [3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5]
pyt1 = Cpu.progInit yt1 (Cpu.Input [])

pi0 = Cpu.progInit i0 (Cpu.Input [])