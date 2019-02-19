module Cipher where

import           Control.Monad
import           Data.Char
import           Data.Char
import           System.Exit   (exitSuccess)

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

consoleCaesar :: IO String
consoleCaesar = do
  putStr "Enter some text to encrypt: "
  text <- getLine
  putStr "Enter number of characters to shift: "
  shiftStr <- getLine
  let shift = read shiftStr :: Int
   in return $ caesar text shift

-- Vigenère cipher
pairUp :: String -> String -> [(Char, Char)]
pairUp [] _ = []
pairUp (x:xs) seq =
  if isSpace x
    then (x, ' ') : pairUp xs seq
    else (x, head seq) : pairUp xs (tail seq)

vig :: String -> String -> String
vig [] _ = ""
vig str salt =
  [ if isLetter (fst pair)
    then uncurry cnv pair
    else fst pair
  | pair <- pairUp str (cycle salt)
  ]
  where
    cnv c nC =
      let ordA = ord 'a'
          ordZ = ord 'z'
          ordIn = ord c
          ordNc = ord nC
          n = ordNc - ordA
          cPlusN = ordIn + n
       in if isSpace c
            then c
            else if cPlusN <= ordZ
                   then chr cPlusN
                   else chr (ordA + (cPlusN - ordZ - 1))

consoleVig :: IO String
consoleVig = do
  putStr "Enter text to encrypt: "
  text <- getLine
  putStr "Enter salt: "
  salt <- getLine
  return $ vig text salt

palindrome :: IO ()
palindrome =
  forever $ do
    putStr "Enter some text to check for palindromity: "
    line1 <- getLine
    let lcLine1 = map toLower $ filter (not . isPunctuation) $ filter (not . isSpace) line1
        revLcLine1 = reverse lcLine1
     in if lcLine1 == revLcLine1
          then putStrLn "It's a palindrome."
          else (do putStrLn "Nope."
                   exitSuccess)
