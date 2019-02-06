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
