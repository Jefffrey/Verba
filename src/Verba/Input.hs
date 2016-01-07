module Verba.Input where

import Verba.Puzzle
import Control.Monad (replicateM)
import Data.Matrix as Matrix

askField :: (Read a) => String -> IO a
askField field = putStrLn field >> getLine >>= (return . read)

askPuzzle :: String -> IO Puzzle
askPuzzle askString = do
    putStrLn askString
    hd <- getLine
    tl <- replicateM (length hd - 1) getLine
    return . Matrix.fromLists . map (map Just) $ hd : tl
