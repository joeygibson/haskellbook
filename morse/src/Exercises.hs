module Exercises where

import           Data.List

mc91 :: Int -> Int
mc91 n
  | n > 100 = n - 10
  | otherwise = mc91 $ mc91 $ n + 11

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"

digits :: Int -> [Int]
digits n = reverse $ go $ divMod n 10
  where
    go :: (Int, Int) -> [Int]
    go (0, r) = [r]
    go (q, r) = r : go (divMod q 10)

wordNumber :: Int -> String
wordNumber = intercalate "-" . map digitToWord . digits

half :: Fractional a => a -> a
half x = x / 2
