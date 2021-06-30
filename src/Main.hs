module Main where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Either
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import System.IO

newtype WordList = WordList [String]
                   deriving (Eq, Show)

-- String: the word to be guessed
-- [Maybe Char]: list of chars that tells you whether or not they have been guessed
-- [Char]: list of guessed chars
data Puzzle =
  Puzzle String [Maybe Char] [Either Char Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = (intersperse ' ' $ fmap renderPuzzleChar discovered)
                                    ++ "     | guessed so far: "
                                    ++ (intersperse ' ' $ allGuesses guessed)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

freshPuzzle :: String -> Puzzle
freshPuzzle str = Puzzle str (map (const Nothing) str) []

allWords :: IO WordList
allWords = do
  dict <- readFile "data/dict.txt"
  return $ WordList (lines dict)

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList (filter criteria aw)
    where criteria w = let l = length (w :: String)
                       in l >= minWordLength && l < maxWordLength && notElem '\'' w

randomWord :: WordList -> IO String
randomWord (WordList wordList) = do
  randomIndex <- randomRIO (0, length wordList)
  return $ wordList !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) char = elem char word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = elem char $ allGuesses guessed

allGuesses :: [Either Char Char] -> [Char]
allGuesses gs = lefts gs ++ rights gs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Either Char Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) l@(Left c) = Puzzle word filledInSoFar (l : s)
fillInCharacter (Puzzle word filledInSoFar s) r@(Right c) = Puzzle word newFilledInSoFar (r : s)
    where zipper guessed wordChar guessChar = if wordChar == guessed then Just wordChar else guessChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> String -> IO Puzzle
handleGuess puzzle g@[guess] = do
  putStrLn $ "Your guess was: " ++ g
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You've already guessed that character - try something else!"
      return puzzle
    (True, _) -> do
      putStrLn "Great! Let me fill in the blanks..."
      return (fillInCharacter puzzle (Right guess))
    (False, _) -> do
      putStrLn "This character is not in this word. Try again."
      return (fillInCharacter puzzle (Left guess))
handleGuess (Puzzle solution filledInSoFar guesses) attempt
  | solution == attempt = return (Puzzle solution (fmap Just solution) guesses)
  | otherwise = return (Puzzle solution filledInSoFar ((Left '_') : guesses))

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ guessed) =
  if (length $ lefts guessed) > 6
  then do putStrLn "You lost! Sorry!"
          putStrLn $ "The word was: " ++ wordToGuess
          exitSuccess
  else return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
  if all isJust filledInSoFar
  then do putStrLn "You won! Good job!!!!"
          exitSuccess
  else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  putStrLn $ "Current puzzle: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  handleGuess puzzle guess >>= runGame

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  runGame puzzle

