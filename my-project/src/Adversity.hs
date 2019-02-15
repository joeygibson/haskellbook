module Adversity where

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
