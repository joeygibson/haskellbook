module Adversity where

import           Data.List
import           Data.Maybe

ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n =
  if even n
    then Just (n + 2)
    else Nothing

type Name = String

type Age = Integer

data Person =
  Person Name
         Age
  deriving (Show)

data PersonInvalid
  = NameEmpty
  | AgeTooLow
  deriving (Eq, Show)

type ValidatePerson a = Either [PersonInvalid] a

-- Using `Maybe`
--mkPerson :: Name -> Age -> Maybe Person
--mkPerson name age
--  | name /= "" && age >= 0 = Just $ Person name age
--  | otherwise = Nothing
--
-- validating functions
ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age =
  if age >= 0
    then Right age
    else Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name =
  if name /= ""
    then Right name
    else Left [NameEmpty]

--
-- using `Either` to indicate _why_ it failed
--mkPerson :: Name -> Age -> Either PersonInvalid Person
--mkPerson name age
--  | name /= "" && age >= 0 = Right $ Person name age
--  | name == "" = Left NameEmpty
--  | age < 0 = Left AgeTooLow
--
-- Using `Either` and validating functions
mkPerson :: Name -> Age -> ValidatePerson Person
mkPerson name age = mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _             = Left badName
mkPerson' _ (Left badAge)              = Left badAge

--
-- Higher-kinded types (HKT)
data Example a
  = Blah
  | Woot a

data Trivial =
  Trivial

data Unary a =
  Unary a
  deriving (Show)

data TwoArgs a b =
  TwoArgs a
          b

data ThreeArgs a b c =
  ThreeArgs a
            b
            c

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe str   = Just str

--notThe text =
--  if text == "the"
--    then Nothing
--    else Just text
--
--replaceThe :: String -> String
--replaceThe "" = ""
--replaceThe text = unwords (go (words text))
--  where
--    go [] = []
--    go (x:xs) =
--      case notThe x of
--        Just word -> word : go xs
--        Nothing -> "a" : go xs
--
--replaceThe :: String -> String
--replaceThe = unwords . go . map notThe . words
--  where
--    go :: [Maybe String] -> [String]
--    go []     = []
--    go (x:xs) = fromMaybe "a" x : go xs
--
replaceThe :: String -> String
replaceThe = unwords . map (fromMaybe "a" . notThe) . words

startsWithVowel :: String -> Bool
startsWithVowel (c:_) = c `elem` "aeiou"

-- my way
--countTheBeforeVowel :: String -> Integer
--countTheBeforeVowel "" = 0
--countTheBeforeVowel text = go (words text)
--  where
--    go :: [String] -> Integer
--    go [] = 0
--    go (x:xs) =
--      if x == "the" && ((`elem` "aeiou") . (!! 0) . head) xs
--        then 1 + go xs
--        else go xs
--
-- another way
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = go . words
  where
    go :: [String] -> Integer
    go [] = 0
    go [_] = 0
    go (w:x:ws) =
      case (w, startsWithVowel x) of
        ("the", True) -> 1 + go ws
        _             -> go (x : ws)

countVowels :: String -> Integer
countVowels =
  foldr
    (\x cnt ->
       if x `elem` "AEIOUaeiou"
         then 1 + cnt
         else cnt)
    0

newtype Word' =
  Word' String
  deriving (Eq, Show)

vowels = "aeiou"

mkWord :: String -> Maybe Word'
mkWord word =
  if numConsonants >= numVowels
    then Just (Word' word)
    else Nothing
  where
    numVowels = countVowels word
    numConsonants = (fromIntegral . length) word - numVowels

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ n) = 1 + natToInteger n

--integerToNat :: Integer -> Maybe Nat
--integerToNat i
--  | i < 0 = Nothing
--  | i == 0 = Just Zero
--  | otherwise = Just (Succ (fromMaybe Zero (integerToNat (i - 1))))
integerToNat :: Integer -> Maybe Nat
integerToNat i
  | i < 0 = Nothing
  | otherwise = Just $ convert i
  where
    convert 0 = Zero
    convert a = Succ $ convert (a - 1)

-- small library for Maybe
isJust :: Maybe a -> Bool
isJust Nothing  = False
isJust (Just _) = True

isNothing :: Maybe a -> Bool
isNothing = not . Adversity.isJust

mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee d _ Nothing  = d
mayybee d f (Just c) = f c

fromMayybee :: a -> Maybe a -> a
fromMayybee d Nothing  = d
fromMayybee _ (Just c) = c

listToMayybe :: [a] -> Maybe a
listToMayybe []    = Nothing
listToMayybe (x:_) = Just x

mayybeToList :: Maybe a -> [a]
mayybeToList Nothing  = []
mayybeToList (Just c) = [c]

-- drop Maybes from list
catMayybes :: [Maybe a] -> [a]
catMayybes = map fromJust . filter Adversity.isJust

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe list =
  if any Adversity.isNothing list
    then Nothing
    else Just $ catMayybes list

-- small library for Either
lefts' :: [Either a b] -> [a]
lefts' xs = [x | Left x <- xs]

-- foldr
--lefts' = foldr takeLeft []
--  where
--    takeLeft (Right _) xs = xs
--    takeLeft (Left x) xs  = x : xs
-- mine
--lefts' []           = []
--lefts' (Left x:xs)  = x : lefts' xs
--lefts' (Right x:xs) = lefts' xs
--
rights' :: [Either a b] -> [b]
rights' xs = [x | Right x <- xs]

--rights' = foldr takeRight []
--  where
--    takeRight (Right x) xs = x : xs
--    takeRight (Left _) xs  = xs
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr part ([], [])
  where
    part (Left l) (ls, rs)  = (l : ls, rs)
    part (Right r) (ls, rs) = (ls, r : rs)

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _)  = Nothing
eitherMaybe' f (Right r) = Just $ f r

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left l)  = f l
either' _ g (Right r) = g r

--eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
--eitherMaybe'' f = either' (const Nothing (Just . f)
--
-- catamorphism - break down a data structure
-- anamorphism - build up a data structure
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x =
  case f x of
    Nothing     -> []
    Just (a, b) -> a : myUnfoldr f b

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\o -> Just (o, f o))

data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

unfoldTree :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfoldTree f x =
  case f x of
    Nothing          -> Leaf
    Just (lt, o, rt) -> Node (unfoldTree f lt) o (unfoldTree f rt)

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree gen 0
  where
    gen x =
      if x == n
        then Nothing
        else Just (x + 1, x, x + 1)
