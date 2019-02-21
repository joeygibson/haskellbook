module Addition where

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where
    go n d count
      | n < d = (count, n)
      | otherwise = go (n - d) d (count + 1)

multiplyBy :: (Eq a, Num a) => a -> a -> a
multiplyBy _ 0 = 0
multiplyBy a 1 = a
multiplyBy a b = a + multiplyBy a (b - 1)
