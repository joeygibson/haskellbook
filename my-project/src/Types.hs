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

-- nullary
trivialValue :: GuessWhat
trivialValue = Chickenbutt

-- unary
idInt :: Id Int
idInt = MkId 23

-- product types
type Awesome = Bool

--type Name = String
person :: Product Name Awesome
person = Product "Frank" True

-- sum types
--data Twitter =
--  Twitter
--  deriving (Eq, Show)
--
--data LastFm =
--  LastFm
--  deriving (Eq, Show)
--socialNetwork :: Sum Twitter LastFm
--socialNetwork = First Twitter
data SocialNetwork
  = Twitter
  | LastFm
  deriving (Eq, Show)

-- record syntax
data OperatingSystem
  = GnuPlusLinux
  | OpenBSD
  | Mac
  | Windows
  deriving (Enum, Eq, Show)

data ProgLang
  = Haskell
  | Agda
  | Idris
  | PureScript
  deriving (Enum, Eq, Show)

data Programmer = Programmer
  { os   :: OperatingSystem
  , lang :: ProgLang
  } deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer {os = Mac, lang = Haskell}

-- you can reorder when using record syntax
feelingWizardly :: Programmer
feelingWizardly = Programmer {lang = Agda, os = GnuPlusLinux}

-- exercises
allOperatingSystems :: [OperatingSystem]
allOperatingSystems = [GnuPlusLinux ..]

allLanguages :: [ProgLang]
allLanguages = [Haskell ..]

allProgammers :: [Programmer]
allProgammers = [Programmer {os = os, lang = lang} | os <- allOperatingSystems, lang <- allLanguages]

-- partial application of constructors
data ThereYet =
  There Float
        Int
        Bool
  deriving (Eq, Show)

nope = There

notYet :: Int -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet 10

yasss :: ThereYet
yasss = notQuite False

-- deconsructing
newtype FarmerName =
  Name String
  deriving (Show)

newtype Acres =
  Acres Int
  deriving (Show)

-- FarmerType is a sum
data FarmerType
  = DairyFarmer
  | WheatFarmer
  | SoybeanFarmer
  deriving (Show)

-- Farmer is a product
data Farmer =
  Farmer FarmerName
         Acres
         FarmerType
  deriving (Show)

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _                        = False

-- product, using record syntax
data FarmerRec = FarmerRec
  { farmerName :: FarmerName
  , acres      :: Acres
  , farmerType :: FarmerType
  } deriving (Show)

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer =
  case farmerType farmer of
    DairyFarmer -> True
    _           -> False

-- Function type is exponential
data Quantum
  = Yes
  | No
  | Both
  deriving (Eq, Show)

-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

-- etc...
-- function type
-- 3 ^ 3
quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes  = Yes
quantFlip1 No   = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes  = Yes
quantFlip2 No   = Yes
quantFlip2 Both = No

-- etc...
-- redefinging List
data List a
  = Nil
  | Cons a
         (List a)

-- binary tree
data BinaryTree a
  = Leaf
  | Node (BinaryTree a)
         a
         (BinaryTree a)
  deriving (Eq, Ord, Show)

insert' :: Ord a => a -> BinaryTree a -> BinaryTree a
insert' b Leaf = Node Leaf b Leaf
insert' b (Node left a right)
  | b == a = Node left a right
  | b < a = Node (insert' b left) a right
  | b > a = Node left a (insert' b right)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf                = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay =
  if mapTree (+ 1) testTree' == mapExpected
    then print "OK"
    else print "nope"

-- convert binary tree to lists
testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

preOrder :: BinaryTree a -> [a]
preOrder Leaf                = []
preOrder (Node left a right) = a : preOrder left ++ preOrder right

inOrder :: BinaryTree a -> [a]
inOrder Leaf                = []
inOrder (Node left a right) = inOrder left ++ [a] ++ inOrder right

postOrder :: BinaryTree a -> [a]
postOrder Leaf                = []
postOrder (Node left a right) = postOrder left ++ postOrder right ++ [a]

-- foldr for BinaryTree
--foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
--foldTree (Node left a right) =