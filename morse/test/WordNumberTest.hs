module WordNumberTest where

import           Data.List
import           Exercises                (digitToWord, digits, half,
                                           wordNumber)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Property

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

listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs = snd $ foldr go (Nothing, True) xs
  where
    go _ status@(_, False) = status
    go y (Nothing, t)      = (Just y, t)
    go y (Just x, t)       = (Just y, x >= y)

prop_listOrdered :: Ord a => [a] -> Bool
prop_listOrdered = listOrdered . sort

prop_plusAssociative x y z = x + (y + z) == (x + y) + z

prop_plusCommutative x y = x + y == y + x

prop_mulAssociative x y z = x * (y * z) == (x * y) * z

prop_mulCommutative x y = x * y == y * x

genPositiveNumbers :: (Eq a, Num a, Arbitrary a) => Gen (a, a)
genPositiveNumbers = do
  a <-
    arbitrary :: (Eq a, Num a, Arbitrary a) =>
                   Gen (NonZero a)
  b <-
    arbitrary :: (Eq a, Num a, Arbitrary a) =>
                   Gen (NonZero a)
  return (getNonZero a, getNonZero b)

myQuotRem x y = quot x y * y + rem x y == x

prop_quotRem :: Property
prop_quotRem = forAll (genPositiveNumbers :: Gen (Int, Int)) $ uncurry myQuotRem

myDivMod x y = div x y * y + mod x y == x

prop_divMod :: Property
prop_divMod = forAll (genPositiveNumbers :: Gen (Int, Int)) $ uncurry myDivMod

prop_powerAssoc :: (Integral a, Eq a) => a -> a -> a -> Bool
prop_powerAssoc x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_powerCommutative :: (Integral a, Eq a) => a -> a -> Bool
prop_powerCommutative x y = x ^ y == y ^ x

otherTests :: IO ()
otherTests =
  hspec $ describe "half" $ it "is the inverse of douling (Float)" $ property (prop_halfIdentity :: Float -> Bool)

orderingTests :: IO ()
orderingTests =
  hspec $
  describe "sort" $ do
    it "puts Ord vales in order (Ordering)" $ property (prop_listOrdered :: [Ordering] -> Bool)
    it "puts Ord values in order (Bool)" $ property (prop_listOrdered :: [Bool] -> Bool)
    it "puts Ord values in order (Int)" $ property (prop_listOrdered :: [Int] -> Bool)
    it "puts Ord values in order (String)" $ property (prop_listOrdered :: [String] -> Bool)

additionTests :: IO ()
additionTests =
  hspec $ do
    describe "add" $ do
      it "does addition associatively (Int)" $ property (prop_plusAssociative :: Int -> Int -> Int -> Bool)
      it "does addition associatively (Integer)" $
        property (prop_plusAssociative :: Integer -> Integer -> Integer -> Bool)
      it "does addition commutatively (Int)" $ property (prop_plusCommutative :: Int -> Int -> Bool)
      it "does addition commutatively (Integer)" $ property (prop_plusCommutative :: Integer -> Integer -> Bool)
    describe "mul" $ do
      it "does multiplication associatively (Int)" $ property (prop_mulAssociative :: Int -> Int -> Int -> Bool)
      it "does multiplication associatively (Integer)" $
        property (prop_mulAssociative :: Integer -> Integer -> Integer -> Bool)
      it "does addition commutatively (Int)" $ property (prop_mulCommutative :: Int -> Int -> Bool)
      it "does addition commutatively (Integer)" $ property (prop_mulCommutative :: Integer -> Integer -> Bool)

divisionTests :: IO ()
divisionTests =
  hspec $
  describe "division" $ do
    it "shows relationship between quot and rem" $ property prop_quotRem
    it "shows relationship between div and mod" $ property prop_divMod

powerTests :: IO ()
powerTests =
  hspec $
  describe "powers" $ do
    it "shows exp is associative" $ expectFailure (prop_powerAssoc :: Int -> Int -> Int -> Bool)
    it "shows exp is commutative" $ expectFailure (prop_powerCommutative :: Int -> Int -> Bool)
