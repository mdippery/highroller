{-|
  Module      : HighRoller.Gaming
  Description : Data types and functions for gaming dice
  License     : ?
  Maintainer  : michael@monkey-robot.com

  Descriptions and behavior of standard multi-sided gaming dice.
-}
module HighRoller.Gaming
  (
    Die,
    die
  ) where

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
