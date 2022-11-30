{-|
  Module      : HighRoller.Gaming
  Description : Data types and functions for gaming dice
  Copyright   : (C) 2022 Michael Dippery
  License     : ?
  Maintainer  : michael@monkey-robot.com

  Descriptions and behavior of standard multi-sided gaming dice.
-}
module HighRoller.Gaming
  (
    -- * Data types
    Rollable(..),
    Die(..),
    die,
    rollable,
    rollableMulti,

    -- * Properties
    sides,
    range,
    expected,
    expectedN,

    -- * Rolling
    roll,
    rollDice,
    rollEach,
    rollIO,
    rollMaybeIO,

    -- * Utilities
    parseRoll,
    replicateDie,
    replicateDice,
    splitDice,
  ) where

import Data.List.Split (splitOn)
import System.Random (RandomGen, newStdGen, randomR)
import Text.Read (readMaybe)

-- | A multi-sided gaming die.
--
-- Create a new die using 'die'.
data Die = D4 | D6 | D8 | D10 | D12 | D20 | D100
  deriving Eq

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

-- | A thing that can be "rolled" to produce a "random" result, like a
-- gaming die in real life.
data Rollable
  = RollableDie Die
  | RollableInt Int
  deriving (Eq, Show)

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

-- | Parses a potential rollable value into a 'Rollable'.
rollable :: String -> Maybe Rollable
rollable s =
  case die s of
    Just d  -> Just (RollableDie d)
    Nothing ->
      case readMaybe s of
        Just n  -> Just (RollableInt n)
        Nothing -> Nothing

-- | Parses a rollable of multiple identical dice into a list of parsed
-- 'Rollable' items.
--
-- For example, "2d10" will return a list consisting of two 'RollableDie'
-- items.
rollableMulti :: String -> [Maybe Rollable]
rollableMulti s =
  case splitDice s of
    Just (n, d) -> map (Just . RollableDie) $ replicateDie n d
    Nothing     ->
      case readMaybe s of
        Just n  -> [(Just . RollableInt) n]
        Nothing -> [Nothing]

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

-- | Copies the second value into a list of the given length.
--
-- In conjunction with 'replicateDice`, this is useful to turn a description
-- like "2d20" into a list of dice.
replicateDie :: Int -> Die -> [Die]
replicateDie = replicate

-- | Turns a description of a dice roll into a list of dice.
--
-- The description is a standard string like "2d20" that can be parsed by
-- 'splitDice' and then turned into a list of individual dice using
-- 'replicateDie'.
replicateDice :: String -> Maybe [Die]
replicateDice s =
  case splitDice s of
    Just (n, d) -> Just (replicateDie n d)
    Nothing     -> Nothing

-- | Expected value of a given die roll.
--
-- This is not simulated but instead is calculated arithmetically.
expected :: Fractional a => Die -> a
expected d =
  let (lo, hi) = range d
   in (fromIntegral lo + fromIntegral hi) / 2

-- | Expected value of /n/ rolls of a given die.
--
-- This is not simulated but instead is calculated arithmetically.
expectedN :: Fractional a => Int -> Die -> a
expectedN n d = sum $ map expected $ replicateDie n d

-- | Number of sides of a given die.
sides :: Die -> Int
sides = read . tail . show

-- | Closed interval of possible rolls for a given die.
range :: Die -> (Int, Int)
range = ((,) 1) . sides

delete :: Char -> String -> String
delete ch = filter (/= ch)

-- | Separates a string representing a roll into individual strings
-- representing each rollable item in that roll.
--
-- Returned items are not guaranteed to be convertible into a 'Rollable'.
parseRoll :: String -> [String]
parseRoll = splitOn "+" . delete ' '

-- | Simulates roll and returns the result.
roll
  :: RandomGen g
  => Rollable   -- ^ Thing to be rolled
  -> g          -- ^ Random number source
  -> (Int, g)   -- ^ Result of the random dice roll and the random number source
roll (RollableDie d) g = randomR (range d) g
roll (RollableInt n) g = (n, g)

-- | Simulates a roll of a die /n/ times and returns the total.
rollDice
  :: RandomGen g
  => Int    -- ^ Number of times to roll the dice
  -> Die    -- ^ Type of die to roll
  -> g      -- ^ Random number source
  -> Int    -- ^ Total result of all /n/ rolls
rollDice = ((sum .) .) . rollEach

-- | Simulates a roll of a die /n/ times and returns each individual result.
rollEach
  :: RandomGen g
  => Int    -- ^ Number of times to roll the die
  -> Die    -- ^ Type of die to roll
  -> g      -- ^ Random number source
  -> [Int]  -- ^ Result of each individual dice roll
rollEach n d g = fst $ foldr go ([], g) $ replicateDie n d
  where
    go :: RandomGen g => Die -> ([Int], g) -> ([Int], g)
    go d' (ls, g') =
      let r   = roll (RollableDie d') g'
          v   = fst r
          g'' = snd r
       in (v : ls, g'')

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
  return $ fst $ roll (RollableDie d) g

-- | Parses the description of a die, rolls it, and returns the result.
--
-- The description of the dice is parsed using 'die' and should be in a
-- form like "d4", "d12", etc. The die is rolled using 'rollIO' and the
-- resulting value is returned. If the description of the die cannot be
-- parsed, 'Nothing' is returned.
--
-- This is a useful function for use in the ghci REPL but probably not of
-- great use outside of it; 'roll' should be preferred.
rollMaybeIO :: String -> IO (Maybe Int)
rollMaybeIO s = do
  case die s of
    Nothing -> return Nothing
    Just d  -> do
      n <- rollIO d
      return $ Just n
