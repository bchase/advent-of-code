module Year2017Spec where

import           Test.Hspec

import           Year2017


spec :: Spec
spec = do
  describe "Day02" $ do
    describe "Day02A" $ do
      let input = "5 1 9 5\n7 5 3\n2 4 6 8"
          day02a' = day02a . day02parse
      it "generates a checksum of the input" $ do
        (day02a' input) `shouldBe` 18
    describe "Day02B" $ do
      let input = "5 9 2 8\n 9 4 7 3\n 3 8 6 5"
          day02b' = day02b . day02parse
      it "generates a checksum of the input" $ do
        (day02b' input) `shouldBe` 9

  describe "Day01" $ do
    describe "Day01A" $ do
      let day01a' = day01a . day01parse
      it "produces 3 for 1122" $ do
        (day01a' "1122") `shouldBe` 3
      it "produces 4 for 1111" $ do
        (day01a' "1111") `shouldBe` 4
      it "produces 0 for 1234" $ do
        (day01a' "1234") `shouldBe` 0
      it "produces 9 for 91212129" $ do
        (day01a' "91212129") `shouldBe` 9
    describe "Day01B'" $ do
      let day01b' = day01b . day01parse
      it "produces 6 for 1212" $ do
        (day01b' "1212") `shouldBe` 6
      it "produces 0 for 1221" $ do
        (day01b' "1221") `shouldBe` 0
      it "produces 4 for 123425" $ do
        (day01b' "123425") `shouldBe` 4
      it "produces 12 for 123123" $ do
        (day01b' "123123") `shouldBe` 12
      it "produces 4 for 12131415" $ do
        (day01b' "12131415") `shouldBe` 4
