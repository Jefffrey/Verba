module Main where

import Verba.Dictionary (Dictionary)
import Verba.Puzzle (Puzzle)
import qualified Verba.Dictionary as Dictionary
import qualified Verba.Puzzle as Puzzle

main :: IO ()
main = do 
    dict <- Dictionary.load 2 "dict/it.dic"
    puz <- Puzzle.ask
    mapM_ (\x -> do
        putStrLn "--------"
        print x
        putStrLn "--------") (Puzzle.consume "sacra" puz)
