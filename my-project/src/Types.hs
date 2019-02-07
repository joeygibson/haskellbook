module Types where

data Trivial =
  Trivial'

-- defining data types
data UnaryTypeCon a =
  UnaryValueCon a

data Pugtype =
  PugData

data HuskyType a =
  HuskyData

data DogueDeBordeaux doge =
  DogueDeBordeaux doge

-- creating valus of types
myPug = PugData :: Pugtype

myHusky :: HuskyType a
myHusky = HuskyData

myOtherHusky :: Num a => HuskyType a
myOtherHusky = HuskyData

myOtherOtherHusky :: HuskyType [[[[Int]]]]
myOtherOtherHusky = HuskyData

myDogue :: DogueDeBordeaux Int
myDogue = DogueDeBordeaux 10

-- doesn't work
--badDogue :: DogueDeBordeaux String
--badDogue = DogueDeBordeaux 10
data Doggies a
  = Husky a
  | Mastiff a
  deriving (Eq, Show)

data Price =
  Price Integer
  deriving (Eq, Show)

data Manufacturer
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)

data Airline
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Size
  = Large
  | Medium
  | Small
  deriving (Eq, Show)

data Vehicle
  = Car Manufacturer
        Price
  | Plane Airline
          Size
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)

urCar = Car Mazda (Price 20000)

clownCar = Car Tata (Price 7000)

doge = Plane PapuAir Small

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _) = True
isPlane _           = False

areCars :: [Vehicle] -> [Bool]
areCars = map isCar

getManu :: Vehicle -> Manufacturer
getManu (Car manufacturer _) = manufacturer

-- cardinality
data Example =
  MakeExample
  deriving (Show) -- nullary

data Example1 =
  MakeExample1 Int
  deriving (Show) -- unary, but should use `newtype` instead

-- unary constructors
newtype Goats =
  Goats Int
  deriving (Eq, Show)

newtype Cows =
  Cows Int
  deriving (Eq, Show)

tooManyGoats :: Goats -> Bool
tooManyGoats (Goats n) = n > 42

class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- custom `tooMany`
--instance TooMany Goats where
--  tooMany (Goats n) = n > 43
-- having a custom type class use its underlying type class instance, by hand
--instance TooMany Goats where
--  tooMany (Goats n) = tooMany n
-- using the `GeneralizedNewtypeDeriving` pragma
--{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--newtype Goats =
--  Goats Int deriving (Eq, Show, TooMany)
-- sum type
data QuantumBool
  = QuantumTrue
  | QuantumFalse
  | QuantumBoth
  deriving (Eq, Show)

-- product type
data TwoQs =
  MkTwoQs QuantumBool
          QuantumBool
  deriving (Eq, Show)

data Person =
  MkPerson String
           Int
  deriving (Eq, Show)

-- same, with record syntax
data BetterPerson = BetterPerson
  { name :: String
  , age  :: Int
  } deriving (Eq, Show)

-- sum of products
data Fiction =
  Fiction
  deriving (Show)

data NonFiction =
  NonFiction
  deriving (Show)

data BookType
  = FictionBook Fiction
  | NonFictionBook NonFiction
  deriving (Show)

type AuthorName = String -- type synonym

data Author =
  Author (AuthorName, BookType)

-- exercises
data FlowerType
  = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving (Show)

type Gardener = String

data Garden =
  Garden Gardener
         FlowerType
  deriving (Show)

-- construction and dconstruction of values
data GuessWhat =
  Chickenbutt
  deriving (Eq, Show)

data Id a =
  MkId a
  deriving (Eq, Show)

data Product a b =
  Product a
          b
  deriving (Eq, Show)

data Sum a b
  = First a
  | Second b
  deriving (Eq, Show)

data RecordProduct a b = RecordProduct
  { pFirst  :: a
  , pSecond :: b
  } deriving (Eq, Show)

newtype NumCow =
  NumCow Int
  deriving (Eq, Show)

newtype NumPig =
  NumPig Int
  deriving (Eq, Show)

data FarmHouse =
  FarmHouse NumCow
            NumPig
  deriving (Eq, Show)

type FarmHouse' = Product NumCow NumPig -- same as FarmHouse

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmHouse =
  BigFarmHouse NumCow
               NumPig
               NumSheep
  deriving (Eq, Show)

type BigFarmhouse' = Product NumCow (Product NumPig NumSheep) -- same as BigFarmhouse

type Name = String

type Age = Int

type LovesMud = Bool

type PoundsOfWool = Int

data CowInfo =
  CowInfo Name
          Age
  deriving (Eq, Show)

data PigInfo =
  PigInfo Name
          Age
          LovesMud
  deriving (Eq, Show)

data SheepInfo =
  SheepInfo Name
            Age
            PoundsOfWool
  deriving (Eq, Show)

data Animal
  = Cow CowInfo
  | Pig PigInfo
  | Sheel SheepInfo
  deriving (Eq, Show)

-- or
type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)
