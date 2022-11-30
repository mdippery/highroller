module HighRoller.GamingSpec where

import Data.Maybe (fromJust, isJust, isNothing)
import System.Random (mkStdGen)
import Test.Hspec

import HighRoller.Gaming

spec :: Spec
spec = do
  describe "die" $ do
    it "parses a valid die description" $ do
      (fromJust . die) "d4" `shouldBe` D4
      (fromJust . die) "d6" `shouldBe` D6
      (fromJust . die) "d8" `shouldBe` D8
      (fromJust . die) "d10" `shouldBe` D10
      (fromJust . die) "d12" `shouldBe` D12
      (fromJust . die) "d20" `shouldBe` D20
      (fromJust . die) "d100" `shouldBe` D100

    it "returns nothing if a die description is not valid" $ do
      (isNothing . die) "" `shouldBe` True
      (isNothing . die) "blah" `shouldBe` True
      (isNothing . die) "D4" `shouldBe` True

  describe "sides" $ do
    it "returns the number of sides of a die" $ do
      sides D4 `shouldBe` 4
      sides D6 `shouldBe` 6
      sides D8 `shouldBe` 8
      sides D10 `shouldBe` 10
      sides D12 `shouldBe` 12
      sides D20 `shouldBe` 20
      sides D100 `shouldBe` 100

  describe "range" $ do
    it "returns the range of a die" $ do
      range D4 `shouldBe` (1, 4)
      range D6 `shouldBe` (1, 6)
      range D8 `shouldBe` (1, 8)
      range D10 `shouldBe` (1, 10)
      range D12 `shouldBe` (1, 12)
      range D20 `shouldBe` (1, 20)
      range D100 `shouldBe` (1, 100)

  describe "replicateDie" $ do
    it "replicates a die" $ do
      replicateDie 4 D4 `shouldBe` [D4, D4, D4, D4]

  describe "replicateDice" $ do
    it "replicates a description of a single die" $ do
      let result = replicateDice "d10"
       in do
         isJust result `shouldBe` True
         fromJust result `shouldBe` [D10]

    it "replicates a description of multiple dice" $ do
      let result = replicateDice "3d10"
       in do
         isJust result `shouldBe` True
         fromJust result `shouldBe` [D10, D10, D10]

    it "does not replicate a description of a single invalid die" $ do
      let result = replicateDice "d19"
       in isNothing result `shouldBe` True

    it "does not replicate a description of multiple invalid dice" $ do
      let result = replicateDice "3d9"
       in isNothing result `shouldBe` True

    it "does not replicate an invalid description" $ do
      let result = replicateDice "blah"
       in isNothing result `shouldBe` True

  describe "expected" $ do
    it "returns the expected value of a die" $ do
      expected D4 `shouldBe` 2.5
      expected D6 `shouldBe` 3.5
      expected D8 `shouldBe` 4.5
      expected D10 `shouldBe` 5.5
      expected D12 `shouldBe` 6.5
      expected D20 `shouldBe` 10.5
      expected D100 `shouldBe` 50.5

  describe "expectedN" $ do
    it "returns the expected value of multiple identical dice" $ do
      expectedN 5 D4 `shouldBe` 12.5
      expectedN 10 D10 `shouldBe` 55.0
      expectedN 8 D6 `shouldBe` 28.0
      expectedN 9 D8 `shouldBe` 40.5
      expectedN 4 D10 `shouldBe` 22.0
      expectedN 10 D12 `shouldBe` 65.0
      expectedN 5 D20 `shouldBe` 52.5
      expectedN 6 D100 `shouldBe` 303.0

  describe "rollDice" $ do
    it "rolls a handful of identical dice and returns the total" $ do
      let g = mkStdGen 11051981
       in rollDice 10 D10 g `shouldBe` 54

  describe "rollEach" $ do
    it "simulates dice rolls and returns each result" $ do
      let g = mkStdGen 11051981
       in rollEach 10 D10 g `shouldBe` [3,10,3,1,8,8,8,2,5,6]

  describe "rollMulti" $ do
    it "simulates a single dice roll and returns the total" $ do
      let g    = mkStdGen 11051981
          vals = fromJust $ splitRoll "d4"
          res  = rollMulti vals g
       in res `shouldBe` 2

    it "simulates a roll of multiple dice and returns the total" $ do
      let g    = mkStdGen 11051981
          vals = fromJust $ splitRoll "2d10"
          res  = rollMulti vals g
       in res `shouldBe` 11

    it "simulates a constant roll and returns the total" $ do
      let g    = mkStdGen 11051981
          vals = fromJust $ splitRoll "9"
          res  = rollMulti vals g
       in res `shouldBe` 9

    it "simulates a complex dice roll and returns the total" $ do
      let g    = mkStdGen 11051981
          vals = fromJust $ splitRoll "2d10 + d4 + 8"
          res  = rollMulti vals g
       in res `shouldBe` 17

  describe "rollEachMulti" $ do
    it "simulates a single dice roll and returns each result" $ do
      let g    = mkStdGen 11051981
          vals = fromJust $ splitRoll "d4"
          res  = rollEachMulti vals g
       in res `shouldBe` [2]

    it "simulates a roll of multiple dice and returns each result" $ do
      let g    = mkStdGen 11051981
          vals = fromJust $ splitRoll "2d10"
          res  = rollEachMulti vals g
       in res `shouldBe` [5,6]

    it "simulates a constant roll and returns each result" $ do
      let g    = mkStdGen 11051981
          vals = fromJust $ splitRoll "9"
          res  = rollEachMulti vals g
       in res `shouldBe` [9]

    it "simulates a complex dice roll and returns each result" $ do
      let g    = mkStdGen 11051981
          vals = fromJust $ splitRoll "2d10 + d4 + 8"
          res  = rollEachMulti vals g
       in res `shouldBe` [2,5,2,8]

  describe "rollIO" $ do
    it "generates a random die roll" $ do
      result <- rollIO D20
      result > 0 `shouldBe` True

  describe "rollMaybeIO" $ do
    it "generates a random die roll when given a valid die description" $ do
      result <- rollMaybeIO "d20"
      isJust result `shouldBe` True
      fromJust result > 0 `shouldBe` True

    it "returns Nothing when given an invalid die description" $ do
      result <- rollMaybeIO "d19"
      isNothing result `shouldBe` True

  describe "splitDice" $ do
    describe "multiple dice" $ do
      it "parses a valid description" $ do
        let res = splitDice "2d8"
         in do
           isJust res `shouldBe` True
           let (n, d) = fromJust res
            in do
              n `shouldBe` 2
              d `shouldBe` D8

      it "does not parse a description with an invalid die" $ do
        isNothing (splitDice "2d9") `shouldBe` True

      it "does not parse a description with an invalid count" $ do
        isNothing (splitDice "zd8") `shouldBe` True

      it "does not parse a description with a negative count" $ do
        isNothing (splitDice "-2d8") `shouldBe` True

    describe "single die" $ do
      it "parses a valid description" $ do
        let res = splitDice "d12"
         in do
           isJust res `shouldBe` True
           let (n, d) = fromJust res
            in do
              n `shouldBe` 1
              d `shouldBe` D12

      it "does not parse an invalid description" $ do
        let res = splitDice "d19"
         in isNothing res `shouldBe` True

    describe "invalid description" $ do
      it "does not parse an invalid description" $ do
        let res = splitDice "blah"
         in isNothing res `shouldBe` True

  describe "parseRoll" $ do
    it "parses the description of a roll" $ do
      parseRoll "2d10 + d8 + 9" `shouldBe` ["2d10", "d8", "9"]

  describe "splitRoll" $ do
    it "parses a single dice roll" $ do
      splitRoll "d10" `shouldBe` Just [RollableDie D10]

    it "parses a multi-dice roll" $ do
      splitRoll "2d10" `shouldBe` Just [RollableDie D10, RollableDie D10]

    it "parses a multi-dice roll with a constant" $ do
      splitRoll "2d10 + 9" `shouldBe` Just [RollableDie D10, RollableDie D10, RollableInt 9]

    it "parses a complex dice roll with a constant" $ do
      splitRoll "2d10 + d4 + 9" `shouldBe` Just [RollableDie D10, RollableDie D10, RollableDie D4, RollableInt 9]

    it "does not parse a roll with an invalid dice" $ do
      splitRoll "2d10 + d4 + d9" `shouldBe` Nothing

    it "does not parse a roll with missing components" $ do
      splitRoll "2d10 +" `shouldBe` Nothing

    it "does not parse a roll with a negative sign" $ do
      splitRoll "2d10 - 2" `shouldBe` Nothing

    it "does not parse an invalid roll" $ do
      splitRoll "blah" `shouldBe` Nothing

    it "does not parse an empty roll" $ do
      splitRoll "" `shouldBe` Nothing

  describe "Rollable" $ do
    describe "rollable" $ do
      it "creates a Rollable Die from a valid string" $ do
        let (RollableDie r) = fromJust $ rollable "d12"
         in r `shouldBe` D12

      it "creates a Rollable Int from a valid string" $ do
        let (RollableInt r) = fromJust $ rollable "9"
         in r `shouldBe` 9

      it "cannot parse an invalid string" $ do
        isNothing (rollable "blah") `shouldBe` True

      it "cannot parse a multi-dice string" $ do
        isNothing (rollable "2d10") `shouldBe`True

    describe "rollableMulti" $ do
      it "creates a list of rollable items from a valid multi-dice string" $ do
        let res = rollableMulti "2d10"
         in res `shouldBe` [Just (RollableDie D10), Just (RollableDie D10)]

      it "creates a list of rollable items from dice string" $ do
        let res = rollableMulti "d10"
         in res `shouldBe` [Just (RollableDie D10)]

      it "creates a list of rollable items from a constant" $ do
        let res = rollableMulti "9"
         in res `shouldBe` [Just (RollableInt 9)]

      it "creates nothing from an invalid string" $ do
        let res = rollableMulti "d9"
         in res `shouldBe` [Nothing]

    describe "Rollable Die" $ do
      describe "roll" $ do
        it "generates a random die roll" $ do
          let g = mkStdGen 11051981
              (result, _) = roll (RollableDie D20) g
           in result `shouldBe` 5

      describe "show" $ do
        it "has a string representation" $ do
          show (RollableDie D20) `shouldBe` "RollableDie d20"

    describe "Rollable Int" $ do
      describe "roll" $ do
        it "returns its value when rolled" $ do
          let g = mkStdGen 11051981
              (result', g') = roll (RollableInt 8) g
              (result'', _) = roll (RollableInt 8) g'
           in do
             result' `shouldBe` 8
             result'' `shouldBe` 8

      describe "show" $ do
        it "has a string representation" $ do
          show (RollableInt 9) `shouldBe` "RollableInt 9"
