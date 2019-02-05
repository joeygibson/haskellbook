module Lists where

brokenMyHead (x:_) = x

brokenMyTail (_:xs) = xs

lessBrokenMyTail :: [a] -> [a]
lessBrokenMyTail []     = []
lessBrokenMyTail (_:xs) = xs

safeTail :: [a] -> Maybe [a]
safeTail []     = Nothing
safeTail (_:[]) = Nothing
safeTail (_:xs) = Just xs

-- list addition
biggerList = [1, 2, 3, 4, 5] ++ [6]

oneToTen = [1 .. 10]

oneToTen' = enumFromTo 1 10

oneToTenUsingThen = enumFromThenTo 1 2 10

oneToTenUsingBrackets = [1,2 .. 10]

oneToTenByTwos = [1,3 .. 10]

twoToTenByTwos = [2,4 .. 10]

letters = ['a' .. 'f']

firstFive = take 5 [1 .. 10] -- [1, 2, 3, 4, 5]

lastFive = drop 5 [1 .. 10] -- [6, 7, 8, 9, 10]

twoLists = splitAt 5 [1 .. 10] -- ([1, 2, 3, 4, 5], [6, 7, 8 , 9, 10])

empty = take 5 [] -- []

lessThanThree = takeWhile (< 3) [1 .. 10] -- [1, 2]

moreThanThree = dropWhile (< 3) [1 .. 10] -- [3, 4, 5, 6, 7, 8, 9, 10]

-- list comprehensions
squares = [x ^ 2 | x <- [1 .. 10]] -- [1,4,9,16,25,36,49,64,81,100]

squaresOfEvens = [x ^ 2 | x <- [1 .. 10], even x] -- [4,16,36,64,100]

exToTheWhy = [x ^ y | x <- [1 .. 5], y <- [2, 3]] -- [1,1,4,8,9,27,16,64,25,125]

pairOfThings = [(x, y) | x <- [1 .. 5], y <- ['a', 'b']] -- [(1,'a'),(1,'b'),(2,'a'),(2,'b'),(3,'a'),(3,'b'),(4,'a'),(4,'b'),(5,'a'),(5,'b')]

found = elem 'a' "fooabar" -- True

tla = [x | x <- "Three Letter Acronym", elem x ['A' .. 'Z']] -- "TLA"

-- TLA generator function
acro xs = [x | x <- xs, elem x ['A' .. 'Z']]

mySqr = [x ^ 2 | x <- [1 .. 5]]

myCub = [x ^ 3 | x <- [1 .. 5]]

sqrAndCub = [(x, y) | x <- mySqr, y <- myCub, x < 50, y < 50]

mySum :: Num a => [a] -> a
mySum []     = 0
mySum (x:xs) = x + mySum xs

--
-- transforming lists
plusOne = map (+ 1) [1 .. 5] -- [2, 3, 4, 5, 6]

minusOne = map (1 -) [1 .. 5] -- [0, -1, -2, -3, -4]

plusOne' = fmap (+ 1) [1 .. 5] -- [2, 3, 4, 5, 6]

-- filtering lists
evens = filter even [1 .. 10] -- [2, 4, 6, 8, 10]

evens' = filter (\x -> rem x 2 == 0) [1 .. 10] -- same

-- zipping lists
aAndB = zip [1, 2, 3] [4, 5, 6] -- [(1, 4), (2, 5), (3, 6)]

(a, b) = unzip $ zip [1, 2, 3] [4, 5, 6] -- ([1,2,3],[4,5,6])

listOfSums = zipWith (+) [1, 2, 3] [4, 5, 6] -- [5, 7, 9]

-- folding lists
sameAsSum = foldr (+) 0 [1 .. 5] -- 15

-- how foldr associates
xs = map show [1 .. 5]

y = foldr (\x y -> concat ["(", x, "+", y, ")"]) "0" xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f xs = foldr (\x b -> f x || b) False xs

-- folding left

