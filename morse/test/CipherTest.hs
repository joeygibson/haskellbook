module CipherTest where

import           Control.Monad
import           Data.Char
import           System.Exit     (exitSuccess)
import           Test.Hspec
import           Test.QuickCheck

--import           Test.QuickCheck.Property
caesar :: String -> Int -> String
caesar [] _ = ""
caesar text n = map cnv text
  where
    cnv c =
      let ordA = ord 'a'
          ordZ = ord 'z'
          ordIn = ord c
          cPlusN = ordIn + n
       in if cPlusN <= ordZ
            then chr cPlusN
            else chr (ordA + (cPlusN - ordZ - 1))

uncaesar :: String -> Int -> String
uncaesar [] _ = ""
uncaesar text n = map cnv text
  where
    cnv c =
      let ordA = ord 'a'
          ordZ = ord 'z'
          ordIn = ord c
          cMinusN = ordIn - n
       in if cMinusN >= ordA
            then chr cMinusN
            else chr (ordZ - (ordA - cMinusN - 1))

--
--prop_caesarProperly :: (Integral b) => String -> Positive b -> Bool
--prop_caesarProperly str (Positive n) =
--  let argN = fromIntegral n
--   in caesar str argN == uncaesar (caesar str argN) argN
prop_caesarIdentity :: String -> Int -> Bool
prop_caesarIdentity s n =
  let n' =
        if n < 1
          then 1
          else n
   in uncaesar (caesar s n') n' == s

arbitraryASCII :: Gen String
arbitraryASCII = getASCIIString <$> arbitrary

prop_caesarPrintIdentity :: Property
--prop_caesarPrintIdentity = forAll arbitraryASCII (forAll (arbitrary :: Gen Int) . prop_caesarIdentity)
prop_caesarPrintIdentity = forAll arbitraryASCII (\s -> forAll (arbitrary :: Gen Int) (\n -> prop_caesarIdentity s n))

cipherTests :: IO ()
cipherTests = hspec $ describe "caesar ciphers" $ it "round-trips" $ property prop_caesarPrintIdentity
