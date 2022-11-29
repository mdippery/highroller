{-|
  Module      : HighRoller.Gaming
  Description : Data types and functions for gaming dice
  License     : ?
  Maintainer  : michael@monkey-robot.com

  Descriptions and behavior of standard multi-sided gaming dice.
-}
module HighRoller.Gaming
  (
    -- * Data types
    Die,
    die,

    -- * Properties
    range,

    -- * Rolling
    roll,
    rollIO,
    rollMaybeIO,

    -- * Utilities
    splitDice,
  ) where

import Data.List.Split (splitOn)
import System.Random (RandomGen, newStdGen, randomR)
import Text.Read (readMaybe)

-- | A multi-sided gaming die.
--
-- Create a new die using 'die'.
data Die = D4 | D6 | D8 | D10 | D12 | D20 | D100

instance Show Die where
  show D4   = "d4"
  show D6   = "d6"
  show D8   = "d8"
  show D10  = "d10"
  show D12  = "d12"
  show D20  = "d20"
  show D100 = "d100"

instance Read Die where
  readsPrec _ = readDie

readDie :: String -> [(Die, String)]
readDie "d4"   = [(D4, "")]
readDie "d6"   = [(D6, "")]
readDie "d8"   = [(D8, "")]
readDie "d10"  = [(D10, "")]
readDie "d12"  = [(D12, "")]
readDie "d20"  = [(D20, "")]
readDie "d100" = [(D100, "")]
readDie _      = []

-- | Parses a standard description of a die into a 'Die'.
--
-- A valid string such as "d4", "d6", etc., will return the corresponding
-- @Die@. 'Nothing' will be returned if the string cannot be parsed into
-- a @Die@.
die :: String -> Maybe Die
die = readMaybe

-- Use `sequence` to parse multiple dice, e.g., 2d4

-- | Parses a description of multiple dice into the 2-tuple
-- (number of dice, die).
--
-- Descriptions such as "2d20" will return the value (2, d20). A simple
-- description like "d20" will return (1, d20). Invalid descriptions will
-- return 'Nothing'.
splitDice :: String -> Maybe (Int, Die)
splitDice s =
  case splitOn "d" s of
    [n, d] ->
      case n of
        "" ->
          case die ('d':d) of
            Just d' -> Just (1, d')
            Nothing -> Nothing
        _  ->
          case die ('d':d) of
            Just d' ->
              case readMaybe n of
                Just n' ->
                  if n' > 0
                     then Just (n', d')
                     else Nothing
                Nothing -> Nothing
            Nothing -> Nothing
    _      -> Nothing

-- | Closed interval of possible rolls for a given die.
range :: Die -> (Int, Int)
range D4   = (1, 4)
range D6   = (1, 6)
range D8   = (1, 8)
range D10  = (1, 10)
range D12  = (1, 12)
range D20  = (1, 20)
range D100 = (1, 100)

-- | Simulates a dice roll and returns the result.
roll
  :: RandomGen g
  => Die           -- ^ Die being rolled
  -> g             -- ^ Random number source
  -> (Int, g)      -- ^ Result of the random dice roll and the random number source
roll = randomR . range

-- | Simulates a random die roll and returns the result.
--
-- Unlike 'roll', this function will select the source of randomnness for you,
-- using 'newStdGen', and return only the value that was rolled on the die.
--
-- This is a useful function for use in the ghci REPL but probably not of
-- great use outside of it; 'roll' should be preferred.
rollIO :: Die -> IO Int
rollIO d = do
  g <- newStdGen
  return $ fst $ roll d g

-- | Parses the description of a die, rolls it, and returns the result.
--
-- The description of the dice is parsed using 'die' and should be in a
-- form like "d4", "d12", etc. The die is rolled using 'rollIO' and the
-- resulting value is returned. If the description of the die cannot be
-- parsed, 'Nothing' is returned.
--
-- This is a useful function for use in the ghci REPL but probably not of
-- great use outside of it; 'roll' should be preferred.
rollMaybeIO :: String -> Maybe (IO Int)
rollMaybeIO s = do
  case die s of
    Nothing -> Nothing
    Just d  -> Just (rollIO d)
