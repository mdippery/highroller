module Main (main) where

import System.Environment (getArgs)
import HighRoller.Gaming

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [desc] ->
      case splitDice desc of
        Just (1, d) -> do
          n <- rollIO d
          putStrLn $ show n
        _           -> return ()
    _      -> return ()
