module Lib
  ( sayHello
  ) where

import           Test.Hspec

sayHello :: IO ()
sayHello = putStrLn "Hello!"
