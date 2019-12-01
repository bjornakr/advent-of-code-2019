module FileParser where

parseLines f filename = do
  inputRaw <- readFile filename
  pure $ fmap f (lines inputRaw)
