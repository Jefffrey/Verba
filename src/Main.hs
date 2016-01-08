module Main where

import Data.List (intercalate)
import Verba.Dictionary (Dictionary)
import Verba.Puzzle (Puzzle)
import qualified Verba.Dictionary as Dictionary
import qualified Verba.Puzzle as Puzzle

reduce :: Dictionary -> [Int] -> (Puzzle, [String]) -> [(Puzzle, [String])]
reduce _ [] puz = [puz]
reduce dict (i:is) (puz, ws) =
    let fn w = map (\x -> (x, w:ws)) (Puzzle.consume w puz)
        puzs = concatMap fn (Dictionary.wordsWithLength i dict) in
    concatMap (reduce dict is) puzs
    

-- Takes a list of word lengths, and a puzzle and
-- generates all strings that result in an empty puzzle.
solve :: Dictionary -> [Int] -> Puzzle -> [[String]]
solve dict is puz = map snd (reduce dict is (puz, []))

main :: IO ()
main = do 
    putStrLn "Puzzle:"
    puz <- Puzzle.ask
    putStrLn "Word lengths (separated by space):"
    lengths <- getLine >>= (return . map read . words)
    dict <- Dictionary.load (maximum lengths) "dict/it.dic"
    let sols = solve dict lengths puz
    mapM_ (putStrLn . intercalate ", ") sols
