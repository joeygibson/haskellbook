module Types where

data Mood
  = Blah
  | Woot
  deriving (Show)

changeMood Blah = Woot
changeMood _ = Blah

greetIfCool :: String -> IO ()
greetIfCool coolness =
  if cool
    then putStrLn "Cool"
    else putStrLn "Square"
  where
    cool = coolness == "frosty"

gicWithFunction :: String -> IO ()
gicWithFunction coolness =
  if cool coolness
    then putStrLn "Cool"
    else putStrLn "Square"
  where
    cool v = v == "frosty"

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome str = (reverse str) == str

myAbs :: Integer -> Integer
myAbs n =
  if n < 0
    then n - (2 * n)
    else n

-- partial application
addStuff :: Integer -> Integer -> Integer
addStuff a b = a + b + 5

addTen = addStuff 5

subtractStuff :: Integer -> Integer -> Integer
subtractStuff x y = x - y - 10

subtractOne = subtractStuff 1

-- decurrying and desugaring...?
nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer -> Bool -> Integer
curriedFunction i b = i + (nonsense b)

uncurriedFunction :: (Integer, Bool) -> Integer
uncurriedFunction (i, b) = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonNested :: Integer -> Bool -> Integer
anonNested = \i -> \b -> i + (nonsense b)

-- type inference
f :: Num a => a -> a -> a
f x y = x + y + 3
