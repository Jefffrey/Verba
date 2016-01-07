module Verba.Input where

import Verba.Puzzle
import Control.Monad (replicateM)
import Data.Matrix as Matrix

-- Asks for a field using the specified label
-- and returns the parsed field.
askField :: (Read a) => String -> IO a
askField field = putStrLn field >> getLine >>= (return . read)

-- Asks for a matrix in the form of rows of strings.
askPuzzle :: String -> IO Puzzle
askPuzzle askString = do
    putStrLn askString
    hd <- getLine
    tl <- replicateM (length hd - 1) getLine
    return . Matrix.fromLists . map (map Just) $ hd : tl
