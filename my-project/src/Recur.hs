module Recur where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

inc :: Num a => a -> a
inc = (+ 1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n     = n
incTimes times n = 1 + incTimes (times - 1) n

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b --applyTimes n f b = f (applyTimes (n - 1) f b)

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times = applyTimes times (+ 1)

--incTimes' times n = applyTimes times (+1) n -- same as above, before eta reduction
-- bottom - when a function doesn't return a value
f :: Bool -> Int
f True  = error "blah"
f False = 0

-- Use `Maybe` to aboid hitting bottom
f' :: Bool -> Maybe Int
f' False = Just 0
f' _     = Nothing

fibonacci :: Integral a => a -> a
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1) + fibonacci (x - 2)

--type Numerator = Integer
--type Denominator = Integer
--type Quotient = Integer
--
--dividedBy :: Numerator -> Denominator -> Quotient
--dividedBy = div
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)
