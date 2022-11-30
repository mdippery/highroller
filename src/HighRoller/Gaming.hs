{-|
  Module      : HighRoller.Gaming
  Description : Data types and functions for gaming dice
  Copyright   : (C) 2022 Michael Dippery
  License     : GPL-3
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
    rollMulti,
    rollEachMulti,
    rollDesc,
    rollIO,
    rollMaybeIO,

    -- * Utilities
    replicateDie,
    replicateDice,
    splitDice,
    parseRoll,
    splitRoll,
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

-- | Parse a standard description of a die into a 'Die'.
--
-- A valid string such as "d4", "d6", etc., will return the corresponding
-- @Die@. 'Nothing' will be returned if the string cannot be parsed into
-- a @Die@.
die :: String -> Maybe Die
die = readMaybe

-- | Parse a potential rollable value into a 'Rollable'.
rollable :: String -> Maybe Rollable
rollable s =
  case die s of
    Just d  -> Just (RollableDie d)
    Nothing ->
      case readMaybe s of
        Just n  -> Just (RollableInt n)
        Nothing -> Nothing

-- | Parse a rollable of multiple identical dice into a list of parsed
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

-- | Parse a description of multiple dice into the 2-tuple
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

-- | Make /n/ copies of the given die.
--
-- In conjunction with 'replicateDice`, this is useful to turn a description
-- like "2d20" into a list of dice.
--
-- ==== __Examples__
--
-- >>> replicateDie 3 D20
-- [D20, D20, D20]
replicateDie :: Int -> Die -> [Die]
replicateDie = replicate

-- | Turn a description of a dice roll into a list of dice.
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

-- | Separate a string representing a roll into individual strings
-- representing each rollable item in that roll.
--
-- Returned items are not guaranteed to be convertible into a 'Rollable'.
parseRoll :: String -> [String]
parseRoll = splitOn "+" . delete ' '

-- | Parse a description of a roll into a list of 'Rollable' items.
--
-- ==== __Examples__
-- >>> splitRoll "d10"
-- Just [RollableDie d10]
-- >>> splitRoll "2d10"
-- Just [RollableDie d10,RollableDie d10]
-- >>> splitRoll "2d10 + 9"
-- Just [RollableDie d10,RollableDie d10,RollableInt 9]
-- >>> splitRoll "2d10 + d9"
-- Nothing
-- >>> splitRoll "2d10 +"
-- Nothing
splitRoll :: String -> Maybe [Rollable]
splitRoll = sequence . concat . map rollableMulti . parseRoll

-- | Simulate roll and return the result.
roll
  :: RandomGen g
  => Rollable   -- ^ Thing to be rolled
  -> g          -- ^ Random number source
  -> (Int, g)   -- ^ Result of the random dice roll and the random number source
roll (RollableDie d) g = randomR (range d) g
roll (RollableInt n) g = (n, g)

-- | Simulate a roll of a die /n/ times and return the total.
rollDice
  :: RandomGen g
  => Int    -- ^ Number of times to roll the dice
  -> Die    -- ^ Type of die to roll
  -> g      -- ^ Random number source
  -> Int    -- ^ Total result of all /n/ rolls
rollDice = ((sum .) .) . rollEach

-- | Simulate a roll of a die /n/ times and return each individual result.
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
      let (v, g'') = roll (RollableDie d') g'
       in (v : ls, g'')

-- | Simulate rolling a sequence of dice and return the total.
rollMulti
  :: RandomGen g
  => [Rollable]   -- ^ Dice being rolled
  -> g            -- ^ Random number source
  -> Int          -- ^ Total result of roll
rollMulti rs g = sum $ rollEachMulti rs g

-- | Simulate rolling a sequence of dice and return each individual
-- result.
rollEachMulti
  :: RandomGen g
  => [Rollable]   -- ^ Dice being rolled
  -> g            -- ^ Random number source
  -> [Int]        -- ^ Result of each individual dice roll
rollEachMulti rs g = fst $ foldr go ([], g) rs
  where
    go :: RandomGen g => Rollable -> ([Int], g) -> ([Int], g)
    go r (acc, g') =
      let (v, g'') = roll r g'
       in (v : acc, g'')

-- | Simulate the roll described by the string.
--
-- The string can be a simple description like "d8" or "2d10", or a more
-- complex description like "2d8 + 1d10 + 4". It can even be a simple
-- constant like "9".
--
-- If the string does not describe a valid roll, 'Nothing' is returned.
rollDesc
  :: RandomGen g
  => String       -- ^ A description of a dice roll
  -> g            -- ^ Random number source
  -> Maybe Int    -- ^ Result of the dice roll if the description is valid
rollDesc s g =
  case splitRoll s of
    Just vals -> Just $ rollMulti vals g
    Nothing   -> Nothing

-- | Simulate a random die roll and return the result.
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

-- | Parse the description of a die, roll it, and return the result.
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
