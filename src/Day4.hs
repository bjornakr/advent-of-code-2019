module Day4 where

import Data.Char (digitToInt)
import Control.Monad
import Control.Monad.Reader
import Data.List

hasAdjacentDups (d1:d2:digits)
  | d1 == d2  = True
  | otherwise = hasAdjacentDups (d2:digits)
hasAdjacentDups _ = False

hasAdjacentDups3 digits =
  let isStrictDup ds = (length ds) == 2 in
  any isStrictDup (group digits)

isAscending (d1:d2:digits)
  | d1 > d2   = False 
  | otherwise = isAscending (d2:digits)
isAscending _ = True

validPasswords = filter (liftM2 (&&) hasAdjacentDups isAscending)
validPasswords3 = filter (liftM2 (&&) hasAdjacentDups3 isAscending)

numberToDigits :: Int -> [Int]
numberToDigits = map digitToInt . show

solve1 = length $ validPasswords (map numberToDigits [246515..739105])
solve3 = length $ validPasswords3 (map numberToDigits [246515..739105])

