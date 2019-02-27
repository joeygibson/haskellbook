module Lib
  ( someFunc
  ) where

import           Data.Monoid
import           Data.Semigroup

someFunc :: IO ()
someFunc = putStrLn "someFunc"

data Booly a
  = False'
  | True'
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Booly a) where
  (<>) False' _    = False'
  (<>) _ False'    = False'
  (<>) True' True' = True'

instance Monoid a => Monoid (Booly a) where
  mempty = False'

data Optional a
  = Nada
  | Only a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (<>) Nada x             = x
  (<>) x Nada             = x
  (<>) (Only a) (Only a') = Only (a <> a')

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
