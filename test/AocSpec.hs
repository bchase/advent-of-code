module AocSpec where

import           Test.Hspec

import           Lib


spec :: Spec
spec = do
  describe "Day01A" $ do
    it "produces 3 for 1122" $ do
      (day01a "1122") `shouldBe` 3
    it "produces 4 for 1111" $ do
      (day01a "1111") `shouldBe` 4
    it "produces 0 for 1234" $ do
      (day01a "1234") `shouldBe` 0
    it "produces 9 for 91212129" $ do
      (day01a "91212129") `shouldBe` 9
