module Day7 where 

validPhaseSettings = [1..4]

allPhaseSettings = do
  a <- validPhaseSettings
  b <- validPhaseSettings
  c <- validPhaseSettings
  d <- validPhaseSettings
  e <- validPhaseSettings
  pure [a,b,c,d,e]

  