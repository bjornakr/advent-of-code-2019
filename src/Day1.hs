module Day1 where

{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Read as TR
import Control.Arrow (left)
import TextShow

data ParseInputError = ParseInputError T.Text deriving (Eq, Show)

parseInput :: T.Text -> Either ParseInputError [Integer]
parseInput input = 
  let
    parseLine line = 
      left (ParseInputError . T.pack) $
      fmap fst $
      TR.decimal line
  in
    traverse parseLine (T.lines input)

calcRequiredFuel :: Integer -> Integer    
calcRequiredFuel mass = (mass `div` 3) - 2

solve :: [Integer] -> Integer
solve = sum . (map (calcRequiredFuelPart2 . fromIntegral))

calcRequiredFuelPart2 :: Integer -> Integer
calcRequiredFuelPart2 x =
  let y = calcRequiredFuel x in
  if y <= 0
  then 0
  else
    y + (calcRequiredFuelPart2 y)

resultToText r = 
  case r of
    Left (ParseInputError err) -> err
    Right is -> showt is

main1 = do
  TIO.putStrLn $ T.pack "=== DAY 1-1 ==="
  inputRaw <- TIO.readFile "src/Day1.dat"
  let input = parseInput inputRaw
  let result = fmap solve input
  printT $ resultToText result

main2 = do
  TIO.putStrLn $ T.pack "=== DAY 1-2 ==="
  inputRaw <- TIO.readFile "src/Day1.dat"
  let input = parseInput inputRaw
  let result = fmap solve input
  printT $ resultToText result

-- main2 =
--   TIO.interact . resultToText . (fmap solve) . parseInput