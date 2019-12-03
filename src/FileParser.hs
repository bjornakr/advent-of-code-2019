module FileParser where

import Data.List.Split (splitOn)

parseLines f filename = do
  inputRaw <- readFile filename
  pure $ fmap f (lines inputRaw)

parseCsv = splitOn ","

parseCsvLines :: (String -> a) -> String -> IO [[a]]
parseCsvLines f filename = do
  lines <- parseLines id filename  
  
  let 
    splits :: [[String]]
    splits = fmap (splitOn ",") lines
  pure $ fmap (fmap f) splits
