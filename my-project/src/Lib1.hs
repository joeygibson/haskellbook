module Lib1
  (
  ) where

add :: (Num a) => a -> a -> a
add x y = x + y

addWeird :: (Num a, Ord a) => a -> a -> a
addWeird x y =
  if x > 1
    then x + y
    else y

myNum :: Num a => a
myNum = 1

bindExp :: Integer -> String
bindExp x =
  let y = 5
   in "The integer was: " ++ show x ++ " and y was: " ++ show y

shadowing :: Integer -> String
shadowing x =
  let x = 10
      y = 5
   in "the int was: " ++ show x ++ " and y was: " ++ show y

anon = (\x -> x * 3) :: Integer -> Integer

-- Pattern matching
isItTwo :: Integer -> Bool
isItTwo 2 = True
isItTwo _ = False

newtype Username =
  Username String

newtype AccountNumber =
  AccountNumber Integer

data User
  = UnregisteredUser
  | RegisteredUser Username
                   AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "Unregistered User"
printUser (RegisteredUser (Username name) (AccountNumber accountNumber)) = putStrLn $ name ++ " " ++ show accountNumber

data WherePenguinsLive
  = Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin =
  Peng WherePenguinsLive
  deriving (Eq, Show)

--isSouthAfrica :: WherePenguinsLive -> Bool
--isSouthAfrica SouthAfrica  = True
--isSouthAfrica Galapagos    = False
--isSouthAfrica Antarctica   = False
--isSouthAfrica Australia    = False
--isSouthAfrica SouthAmerica = False
isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _           = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereItLives) = whereItLives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _                = False

antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _                 = False

antarcticOrGalapagosPenguin :: Penguin -> Bool
antarcticOrGalapagosPenguin p = galapagosPenguin p || antarcticPenguin p

addEmUp2 :: Num a => (a, a) -> a
--addEmUp2 tup = (fst tup) + (snd tup) -- could be written this way
addEmUp2 (x, y) = x + y

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

third3 :: (a, b, c) -> c
third3 (_, _, z) = z

-- case expressions
funcZ x =
  case x + 1 == 1 of
    True  -> "Yes"
    False -> "No"

pal xs =
  case xs == reverse xs of
    True  -> "It is"
    False -> "It is not"

pal1 xs =
  case y of
    True  -> "it is"
    False -> "it is not"
  where
    y = xs == reverse xs

myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x

data Employee
  = Coder
  | Manager
  | Veep
  | CEO
  deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' = putStrLn $ show e ++ " is the boss of " ++ show e'

--employeeRank :: Employee -> Employee -> IO ()
--employeeRank e e' =
--  case compare e e' of
--    GT -> reportBoss e e'
--    EQ -> putStrLn "Neither is the boss"
--    LT -> reportBoss e' e
employeeRank :: (Employee -> Employee -> Ordering) -> Employee -> Employee -> IO ()
employeeRank f e e' =
  case f e e' of
    GT -> reportBoss e e'
    EQ -> putStrLn "Neither is the boss"
    LT -> flip reportBoss e e'

codersRule :: Employee -> Employee -> Ordering
codersRule Coder Coder = EQ
codersRule Coder _     = GT
codersRule _ Coder     = LT
codersRule e e'        = compare e e'

-- guards
myAbs :: Integer -> Integer
myAbs x
  | x < 0 = -x
  | otherwise = x

bloodNA :: Integer -> String
bloodNA x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a ^ 2 + b ^ 2 == c ^ 2 = "right"
  | otherwise = "not right"

dogYears :: Integer -> Integer
dogYears x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6

-- `where` declarations with guards
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | otherwise = 'F'
  where
    y = x / 100

-- function composition using `.`
-- functions are applied from _right_ to _left_
--
negatedSum = negate . sum $ [1 .. 5] -- -15

negatedSum' = negate (sum [1 .. 5]) -- same as above
reversedTopFive = take 5 . reverse $ [1..10] -- [10,9,8,7,6]

-- pointfree style
-- f = negate . sum
--
--add' :: Int -> Int -> Int
--add' x y = x + y
--
--addPF :: Int -> Int -> Int
--addPF = (+)
--
--addOne :: Int -> Int
--addOne = \x -> x + 1
--
--addOnePF :: Int -> Int
--addOnePF = (+1)
--

myPrint :: (Show a) => a -> IO ()
myPrint x = (putStrLn . show) x

myPrintPointFree :: Show a => a -> IO ()
myPrintPointFree = putStrLn . show

-- curry exaple
addAgain :: (Int, Int) -> Int
addAgain (x, y) = x + y

addAgainCurried = curry addAgain
