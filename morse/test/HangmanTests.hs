module HangmanTests where

import           Control.Monad            (forever, when)
import           Data.Char                (toLower)
import           Data.List                (intersperse, sort)
import           Data.Maybe               (isJust)
import           System.Exit              (exitSuccess)
import           System.Random            (randomRIO)
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Property

newtype WordList =
  WordList [String]
  deriving (Eq, Show)

data Puzzle =
  Puzzle String
         [Maybe Char]
         [Char]
  deriving (Eq)

instance Show Puzzle where
  show (Puzzle _ discovered guessed) =
    (intersperse ' ' $ fmap renderPuzzleChar discovered) ++ " Guessed so far: " ++ sort guessed

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 7

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter gameLength aw)
  where
    gameLength w =
      let l = length (w :: String)
       in l >= minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  randomIndex <- randomRIO (0, length wl)
  return $ wl !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = char `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
--renderPuzzleChar = fromMaybe '_'
renderPuzzleChar Nothing  = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) c = Puzzle word newFilledInSoFar (c : s)
  where
    zipper guessed wordChar guessChar =
      if wordChar == guessed
        then Just wordChar
        else guessChar
    newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
  putStrLn $ "Your guess was: " ++ [guess]
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else"
      return puzzle
    (True, _) -> do
      putStrLn "This character was in the word"
      return (fillInCharacter puzzle guess)
    (False, _) -> do
      putStrLn "This character wasn't in the word. Try again."
      return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess filledInSoFar guessed) =
  Control.Monad.when (length guessed - length filledInSoFar > 10) $ do
    putStrLn "You lose."
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess

gameWin :: Puzzle -> IO ()
gameWin puzzle@(Puzzle _ filledInSoFar _) =
  when (all isJust filledInSoFar) $ do
    putStrLn "You win!"
    putStrLn $ show puzzle
    exitSuccess

runGame :: Puzzle -> IO ()
runGame puzzle =
  forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
      [c] -> handleGuess puzzle c >>= runGame
      _   -> putStrLn "Your guess must be a single character."

-- test functions
blankPuzzle :: Puzzle
blankPuzzle = freshPuzzle ""

prop_addToBlank :: Char -> Bool
prop_addToBlank char = fillInCharacter blankPuzzle char == Puzzle "" [] [char]

hangmanTests :: IO ()
hangmanTests =
  hspec $
  describe "fillInChar" $ do
    context "on a blank puzzle" $ it "adds a guessed letter" $ property prop_addToBlank
    context "on a puzzle with a matching letter" $
      it "fills in the correct slots" $
      fillInCharacter (freshPuzzle "abc") 'c' `shouldBe` Puzzle "abc" [Nothing, Nothing, Just 'c'] "c"
    context "on a puzzle with no matching letter" $
      it "fills in the correct slot" $
      fillInCharacter (freshPuzzle "abc") 'd' `shouldBe` Puzzle "abc" (replicate 3 Nothing) "d"
