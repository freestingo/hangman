module Ascii where

import Model

import Data.List (intercalate)
import Data.Either

-- TODO for the love of god, find a smarter way to do this
currentHangedMan :: Puzzle -> [String]
currentHangedMan (Puzzle _ _ guesses) = case length $ lefts guesses of
  0 -> [ "___________"
       , "|         |"
       , "|"
       , "|"
       , "|"
       , "|"
       , "|"
       , "|"
       , "|"
       , "|"
       , "-------------------"
       ]
  1 -> [ "___________"
       , "|         |"
       , "|         O"
       , "|"
       , "|"
       , "|"
       , "|"
       , "|"
       , "|"
       , "|"
       , "-------------------"
       ]
  2 -> [ "___________"
       , "|         |"
       , "|         O"
       , "|         |"
       , "|         |"
       , "|         |"
       , "|"
       , "|"
       , "|"
       , "|"
       , "-------------------"
       ]
  3 -> [ "___________"
       , "|         |"
       , "|         O"
       , "|        /|"
       , "|       / |"
       , "|         |"
       , "|"
       , "|"
       , "|"
       , "|"
       , "-------------------"
       ]
  4 -> [ "___________"
       , "|         |"
       , "|         O"
       , "|        /|\\"
       , "|       / | \\"
       , "|         |"
       , "|"
       , "|"
       , "|"
       , "|"
       , "-------------------"
       ]
  5 -> [ "___________"
       , "|         |"
       , "|         O"
       , "|        /|\\"
       , "|       / | \\"
       , "|         |"
       , "|        /"
       , "|       /"
       , "|"
       , "|"
       , "-------------------"
       ]
  6 -> [ "___________"
       , "|         |"
       , "|         O"
       , "|        /|\\"
       , "|       / | \\"
       , "|         |"
       , "|        / \\"
       , "|       /   \\"
       , "|"
       , "|"
       , "-------------------"
       ]
  _ -> []

banner :: [String]
banner = [ "██╗  ██╗ █████╗ ███╗   ██╗ ██████╗ ███╗   ███╗ █████╗ ███╗   ██╗"
         , "██║  ██║██╔══██╗████╗  ██║██╔════╝ ████╗ ████║██╔══██╗████╗  ██║"
         , "███████║███████║██╔██╗ ██║██║  ███╗██╔████╔██║███████║██╔██╗ ██║"
         , "██╔══██║██╔══██║██║╚██╗██║██║   ██║██║╚██╔╝██║██╔══██║██║╚██╗██║"
         , "██║  ██║██║  ██║██║ ╚████║╚██████╔╝██║ ╚═╝ ██║██║  ██║██║ ╚████║"
         , "╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝"
         ]

showAscii :: [String] -> IO ()
showAscii = putStrLn . intercalate "\n"
