module HighRoller.GamingSpec where

import Data.Maybe (fromJust, isNothing)
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
