module Main (main) where

import System.Environment (getArgs)
import System.Random (newStdGen)
import HighRoller.Gaming hiding (expected)

main :: IO ()
main = do
  argv <- getArgs
  case argv of
    [desc] ->
      case splitDice desc of
        Nothing     -> return ()
        Just (n, d) -> do
          g <- newStdGen
          let actual = rollDice n d g
              expected = expectedN n d :: Double
           in putStrLn $ (show actual) ++ " (expected " ++ (show expected) ++")"
    _      -> return ()
