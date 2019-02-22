module WordNumberTest where

import           Exercises       (digitToWord, digits, half, wordNumber)
import           Test.Hspec
import           Test.QuickCheck

wordNumberTests :: IO ()
wordNumberTests =
  hspec $ do
    describe "digitToWord" $ do
      it "returns zero for 0" $ digitToWord 0 `shouldBe` "zero"
      it "returns one for 1" $ digitToWord 1 `shouldBe` "one"
    describe "digits" $ do
      it "returns [1] for 1" $ digits 1 `shouldBe` [1]
      it "returns [1, 0, 0] for 100" $ digits 100 `shouldBe` [1, 0, 0]
    describe "wordNumber" $ do
      it "returns one-zero-zero given 100" $ wordNumber 100 `shouldBe` "one-zero-zero"
      it "returns nine-zero-zero-one given 9001" $ wordNumber 9001 `shouldBe` "nine-zero-zero-one"

halfIdentity :: Fractional a => a -> a
halfIdentity = (* 2) . half

prop_halfIdentity :: (Eq a, Fractional a) => a -> Bool
prop_halfIdentity x = halfIdentity x == x

otherTests :: IO ()
otherTests =
  hspec $ describe "half" $ it "is the inverse of douling (Float)" $ property (prop_halfIdentity :: Float -> Bool)

