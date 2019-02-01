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

exToTheWhy = [x^y| x <- [1..5], y <- [2, 3]] -- [1,1,4,8,9,27,16,64,25,125]
