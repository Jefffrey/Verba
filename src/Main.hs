module Main where

import Verba.Dictionary (Dictionary)
import Verba.Puzzle (Puzzle)
import qualified Verba.Dictionary as Dictionary
import qualified Verba.Puzzle as Puzzle

main :: IO ()
main = do 
    dict <- Dictionary.load 2 "dict/it.dic"
    let puz = Puzzle.fromLists [ [Just 'a', Just 'e', Just 'o', Just 'a']
                               , [Just 't', Just 't', Just 's', Just 'c']
                               , [Just 'a', Just 'a', Just 'r', Just 'a']
                               , [Just 'p', Just 'o', Just 'c', Just 'e']
                               ]
    mapM_ (\x -> do
        putStrLn "--------"
        print x
        putStrLn "--------") (Puzzle.consume "sacra" puz)
