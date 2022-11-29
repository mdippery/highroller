module HighRoller.GamingSpec where

import Data.Maybe (fromJust, isJust, isNothing)
import System.Random (mkStdGen)
import Test.Hspec

import HighRoller.Gaming

spec :: Spec
spec = do
  describe "die" $ do
    it "parses a valid die description" $ do
      (show . fromJust . die) "d4" `shouldBe` "d4"
      (show . fromJust . die) "d6" `shouldBe` "d6"
      (show . fromJust . die) "d8" `shouldBe` "d8"
      (show . fromJust . die) "d10" `shouldBe` "d10"
      (show . fromJust . die) "d12" `shouldBe` "d12"
      (show . fromJust . die) "d20" `shouldBe` "d20"
      (show . fromJust . die) "d100" `shouldBe` "d100"

    it "returns nothing if a die description is not valid" $ do
      (isNothing . die) "" `shouldBe` True
      (isNothing . die) "blah" `shouldBe` True
      (isNothing . die) "D4" `shouldBe` True

  describe "range" $ do
    it "returns the range of a die" $ do
      range ((fromJust . die) "d4") `shouldBe` (1, 4)
      range ((fromJust . die) "d6") `shouldBe` (1, 6)
      range ((fromJust . die) "d8") `shouldBe` (1, 8)
      range ((fromJust . die) "d10") `shouldBe` (1, 10)
      range ((fromJust . die) "d12") `shouldBe` (1, 12)
      range ((fromJust . die) "d20") `shouldBe` (1, 20)
      range ((fromJust . die) "d100") `shouldBe` (1, 100)

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

  describe "roll" $ do
    it "generates a random die roll" $ do
      let g = mkStdGen 11051981
          (result, _) = roll ((fromJust . die) "d20") g
       in result `shouldBe` 5

  describe "rollIO" $ do
    it "generates a random die roll" $ do
      result <- rollIO ((fromJust . die) "d20")
      result > 0 `shouldBe` True

  describe "rollMaybeIO" $ do
    it "generates a random die roll when given a valid die description" $ do
      let result = rollMaybeIO "d20"
       in do
         isJust result `shouldBe` True
         result' <- fromJust result
         result' > 0 `shouldBe` True

    it "returns Nothing when given an invalid die description" $ do
      let result = rollMaybeIO "d19"
       in isNothing result `shouldBe` True

  describe "splitDice" $ do
    describe "multiple dice" $ do
      it "parses a valid description" $ do
        let res = splitDice "2d8"
         in do
           isJust res `shouldBe` True
           let (n, d) = fromJust res
            in do
              n `shouldBe` 2
              show d `shouldBe` "d8"

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
              show d `shouldBe` "d12"

      it "does not parse an invalid description" $ do
        let res = splitDice "d19"
         in isNothing res `shouldBe` True

    describe "invalid description" $ do
      it "does not parse an invalid description" $ do
        let res = splitDice "blah"
         in isNothing res `shouldBe` True
