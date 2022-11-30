module Main (main) where

import Data.List (intercalate)
import System.Environment (getArgs)
import System.Random (newStdGen)
import HighRoller.Gaming hiding (expected)

main :: IO ()
main = do
  argv <- getArgs
  g <- newStdGen
  case argv of
    [] -> return ()
    ls ->
      case rollDesc (intercalate " " ls) g of
        Just val -> putStrLn $ show val
        Nothing  -> return ()
