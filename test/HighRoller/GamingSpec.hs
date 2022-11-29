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
