module Day9 where

import qualified IntCodeCpu as Cpu

t0 = [109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99]
pt0 = Cpu.progInit t0 $ Cpu.Input []
