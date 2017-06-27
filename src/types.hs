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