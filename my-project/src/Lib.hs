module Lib
  ( someFunc
  ) where

import           Data.List
import           Data.Tuple

someFunc :: IO ()
someFunc = putStrLn "XXX"

topLevelFunction :: Integer -> Integer
topLevelFunction x = x + woot + topLevelValue
  where
    woot :: Integer
    woot = 10

topLevelValue :: Integer
topLevelValue = 5

consOperator = 'f' : "rank" -- "Frank"

headOperator = head [1, 2, 3] -- 1

tailOperator = tail [1, 2, 3] -- [2, 3]

takeOne = take 1 "testing" -- "t"

-- takeZero = take 0 "testing" -- ""
dropOne = drop 1 "testing" -- "esting"

dropFour = drop 4 "testing" -- "ing"

drop999 = drop 999 "testing" -- ""

indexOne = "testing" !! 1 -- 'e'

-- head "" -- *** Exception: Prelude.head: empty list
-- "" !! 4 -- *** Exception: Prelude.!!: index too large
thirdLetter :: String -> Char
thirdLetter x = x !! 3

data Mood
  = Blah
  | Woot
  deriving (Show)

changeMood Blah = Woot
changeMood _    = Blah

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "eyyyyy. What's shakin'?"
    else putStrLn "psshhhhh."
  where
    cool = coolness == "downright frosty, yo"

greetIfCool2 :: String -> IO ()
greetIfCool2 coolness =
  if cool coolness
    then putStrLn "true"
    else putStrLn "false"
  where
    cool v = v == "frosty"

tupleA = (1, 2)

tupleB = swap tupleA -- `swap` is in `Data.Tuple`

-- pattern matching
fst' :: (a, b) -> a
fst' (a, b) = a

snd' :: (a, b) -> b
snd' (a, b) = b

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome orig = orig == rev
  where
    rev = reverse orig

myAbs :: Integer -> Integer
myAbs i =
  if i >= 0
    then i
    else -i

--length' :: [a] -> Integer
--length' [] = 0
--length' x :: xs = 1 + length' xs
addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b

f :: Num a => a -> a -> a
f x y = x + y + 3

calcChange :: (Num a, Ord a) => a -> a -> a
calcChange owed given =
  if change > 0
    then change
    else 0
  where
    change = given - owed

sumSquareOrSquareSum :: (Num a, Ord a) => a -> a -> a
sumSquareOrSquareSum x y =
  if sumSquare > squareSum
    then sumSquare
    else squareSum
  where
    sumSquare = x ^ 2 + y ^ 2
    squareSum = (x + y) ^ 2

ifEven f x =
  if even x
    then f x
    else x

--compareLastNames name1 name2 =
--  if lastName1 > lastName2
--    then GT
--    else if lastName1 < lastName2
--           then LT
--           else EQ
--  where
--    lastName1 = snd name1
--    lastName2 = snd name2
compareLastNames name1 name2
  | lastName1 > lastName2 = GT
  | lastName1 < lastName2 = LT
  | otherwise = EQ
  where
    lastName1 = snd name1
    lastName2 = snd name2

newOrder = [("Ian", "Curtis"), ("Peter", "Hook"), ("Stephen", "Morris"), ("Bernard", "Sumner")]

sortedNewOrder = sortBy compareLastNames newOrder

-- creating an instance of Eq for a custom type
data Trivial =
  Trivial'

instance Eq Trivial where
  Trivial' == Trivial' = True

data DayOfWeek
  = Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  | Sun
  deriving (Ord, Show)

data Date =
  Date DayOfWeek
       Int
  deriving (Show)

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Wed Wed = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

instance Eq Date where
  (==) (Date weekday dayOfMonth) (Date weekday1 dayOfMonth1) = weekday == weekday1 && dayOfMonth == dayOfMonth1

data Identity a =
  Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

-- Change ordering so Fri is greater than any other day
--instance Ord DayOfWeek where
--  compare Fri Fri = EQ
--  compare Fri _   = GT
--  compare _ Fri   = LT
--  compare _ _     = EQ
class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where
    integerOfA = toNumber a
    integerOfA' = toNumber a'
    summed = integerOfA + integerOfA'
