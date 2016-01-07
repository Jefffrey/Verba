module Main where

import Verba.Input

main :: IO ()
main = askPuzzle "Matrix:" >>= print
