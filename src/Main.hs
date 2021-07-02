module Main where

import Ascii
import Logic
import Model

import System.Console.ANSI
import Data.Char (toLower)
import System.IO

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  setTitle "Hangman!"
  word <- randomWord'
  let puzzle = freshPuzzle (fmap toLower word)
  clearAllScreen
  showAscii banner
  runGame puzzle

