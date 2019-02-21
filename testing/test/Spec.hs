import           Addition
import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a' .. 'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [(1, return Nothing), (3, return (Just a))]

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater

main :: IO ()
main =
  hspec $ do
    describe "Prelude.head" $ do
      it "returns the first element of a list" $ head [23 ..] `shouldBe` (23 :: Int)
      it "returns the first elemenet of an *arbitrary* list" $ property $ \x xs -> head (x : xs) == (x :: Int)
      it "throws an exception if used with an empty list" $ evaluate (head []) `shouldThrow` anyException
      it "x + 1 is always greater than 1" $ property $ \x -> x + 1 > (x :: Int)
    describe "Addition.dividedBy" $ it "22 dividedBy 5 is 4 rem 2" $ dividedBy 22 5 `shouldBe` (4, 2)
    describe "Addition.multiplyBy" $ it "5 multiplyBy 6 is 30" $ multiplyBy 5 6 `shouldBe` 30
