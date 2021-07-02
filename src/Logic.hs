module Logic where

import Model
import Ascii

import Data.Either
import Data.Maybe (isJust)
import System.Console.ANSI
import System.Exit (exitSuccess)
import System.Random (randomRIO)
import Control.Monad (forever, when)

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
charInWord (Puzzle word _ _) char = char `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) char = elem char $ allGuesses guessed

fillInCharacter :: Puzzle -> Either Char Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar s) l@(Left c) = Puzzle word filledInSoFar (l : s)
fillInCharacter (Puzzle word filledInSoFar s) r@(Right c) = Puzzle word newFilledInSoFar (r : s)
    where zipper guessed wordChar guessChar = if wordChar == guessed then Just wordChar else guessChar
          newFilledInSoFar = zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> String -> IO Puzzle
handleGuess puzzle g@[guess] = do
  case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
    (_, True) -> do
      putStrLn "You've already guessed that character - try another one!"
      return puzzle
    (True, _) -> do
      putStrLn "Great! Let me fill in the blanks..."
      return (fillInCharacter puzzle (Right guess))
    (False, _) -> do
      putStrLn $ "This word does not contain any " ++ g ++ "'s."
      return (fillInCharacter puzzle (Left guess))
handleGuess (Puzzle solution filledInSoFar guesses) attempt
  | solution == attempt = return (Puzzle solution (fmap Just solution) guesses)
  | otherwise = do
      putStrLn $ attempt ++ " is not the correct word!"
      return (Puzzle solution filledInSoFar (Left '_' : guesses))

gameOver :: Puzzle -> IO ()
gameOver puzzle@(Puzzle wordToGuess _ guessed) =
  when (length (lefts guessed) > maxWrongGuesses) $ do
    showAscii $ currentHangedMan puzzle
    putStrLn "You lost! Sorry!"
    putStrLn $ "The word was: " ++ wordToGuess
    exitSuccess


gameWin :: Puzzle -> IO ()
gameWin (Puzzle guessedWord filledInSoFar _) =
  when (all isJust filledInSoFar) $ do
    putStrLn "You won! Good job!!!!"
    putStrLn $ "The word was: " ++ guessedWord
    exitSuccess

resetScreen :: IO ()
resetScreen = do
  setCursorPosition (length banner) 0
  clearFromCursorToScreenEnd

clearAllScreen :: IO ()
clearAllScreen = do
  clearScreen
  setCursorPosition 0 0

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameWin puzzle
  gameOver puzzle
  showAscii $ currentHangedMan puzzle
  putStrLn $ "Current puzzle: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  resetScreen
  handleGuess puzzle guess >>= runGame
