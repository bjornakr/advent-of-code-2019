module Day4 where

import Data.Char (digitToInt)
import Control.Monad
import Control.Monad.Reader

hasAdjacentDups (d1:d2:digits)
  | d1 == d2  = True
  | otherwise = hasAdjacentDups (d2:digits)
hasAdjacentDups _ = False

hasAdjacentDups2 found (d1:d2:digits)
  | d1 == d2  = hasAdjacentDups2 True (d2:digits)
  | found  = True 
  | otherwise = hasAdjacentDups2 False (d2:digits)
hasAdjacentDups2 found _ = found

isAscending (d1:d2:digits)
  | d1 > d2   = False 
  | otherwise = isAscending (d2:digits)
isAscending _ = True

validPasswords = filter (liftM2 (&&) hasAdjacentDups isAscending)
validPasswords2 = filter (liftM2 (&&) (hasAdjacentDups2 False) isAscending)

numberToDigits :: Int -> [Int]
numberToDigits = map digitToInt . show

solve1 = length $ validPasswords (map numberToDigits [246515..739105])
solve2 = length $ validPasswords2 (map numberToDigits [246515..739105])

