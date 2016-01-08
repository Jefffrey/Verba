module Verba.Solver (solve) where

import Data.List (permutations)
import Verba.Dictionary (Dictionary)
import Verba.Puzzle (Puzzle)
import qualified Verba.Dictionary as Dictionary
import qualified Verba.Puzzle as Puzzle

-- Reduces to a list of puzzles starting from
-- the given puzzle.
reduce :: Dictionary -> [Int] -> (Puzzle, [String]) -> [(Puzzle, [String])]
reduce _ [] puz = [puz]
reduce dict (i:is) (puz, ws) =
    let fn w = map (\x -> (x, w:ws)) (Puzzle.consume w puz)
        puzs = concatMap fn (Dictionary.wordsWithLength i dict) in
    concatMap (reduce dict is) puzs
    

-- Takes a list of word lengths, and a puzzle and
-- generates all strings that result in an empty puzzle.
solve :: Dictionary -> [Int] -> Puzzle -> [[String]]
solve dict is puz = 
    let allLengths = permutations is in
    concatMap (\ls -> map (reverse . snd) (reduce dict ls (puz, []))) allLengths
