module Day1r where

  import FileParser

  calcRequiredFuel1 mass = (mass `div` 3) - 2
  
  calcRequiredFuel2 x =
    let y = calcRequiredFuel1 x in
    if y <= 0
    then 0
    else y + (calcRequiredFuel2 y)
  
  solve f xs = sum $ map f xs
  
  main1 = do
    putStrLn "=== DAY 1-1 ==="
    input <- parseLines read "src/Day1.dat"
    let result = solve calcRequiredFuel1 input
    putStrLn $ show result
  
  main2 = do
    putStrLn "=== DAY 1-2 ==="
    input <- parseLines read "src/Day1.dat"
    let result = solve calcRequiredFuel2 input
    putStrLn $ show result
