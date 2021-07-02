module Model where

import Data.List (intersperse, intercalate)
import Data.Either

newtype WordList = WordList [String]
                   deriving (Eq, Show)

{-|
   A Puzzle represents all the necessary data for a game of Hangman.

   It contains the word to be guessed, a list of chars that have already been guessed
   or that are undiscovered yet, and a list of either successful or wrong guesses.
-}
data Puzzle =
  Puzzle String [Maybe Char] [Either Char Char]

instance Show Puzzle where
  show (Puzzle _ discovered guessed) = intersperse ' ' (fmap renderPuzzleChar discovered)
                                    ++ "     | guessed so far: "
                                    ++ intersperse ' ' (allGuesses guessed)

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

maxWrongGuesses :: Int
maxWrongGuesses = 5

allGuesses :: [Either Char Char] -> [Char]
allGuesses gs = lefts gs ++ rights gs

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

