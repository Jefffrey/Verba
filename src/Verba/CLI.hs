module Verba.CLI (runCLI) where

import Data.List (intersperse, intercalate)
import System.IO (hFlush, stdout)
import Verba.Solver (solve)
import Verba.Dictionary (Dictionary)
import Verba.Puzzle (Puzzle)
import Verba.Formatting
import Verba.Utils
import qualified Verba.Dictionary as Dictionary
import qualified Verba.Puzzle as Puzzle

runCLI :: FilePath -> IO ()
runCLI dictFile = do
    putStrLn $ blue "Matrix (row by row):"
    puz <- Puzzle.ask
    putStr $ blue "Word lengths (separated by space): "
    hFlush stdout
    lengths <- getLine >>= (return . map read . words)
    putStr $ blue "Known correct words (separated by space): "
    hFlush stdout
    knownWords <- getLine >>= (return . words)
    dict <- Dictionary.load (maximum lengths) dictFile
    let sols = solve dict lengths puz
    putStrLn "---------------------------------"
    let initFilter = filter (supersetOf knownWords) sols
    confirmSolutions (length lengths) knownWords initFilter

putGuess :: String -> [String] -> [String] -> IO ()
putGuess guessedWord correctOnes guess = do
    let styledWords = map (\w -> if (w `elem` correctOnes)
            then green w
            else if w == guessedWord
                then orange w
                else w) guess
    putStr $ (intercalate " " styledWords) ++ "? "

-- Takes a list of known solutions and a 
-- list of grouped solutions (pair of first word and 
-- rest of the solutions) and returns the list of confirmed solutions
-- going deep as long as the user confirms.
-- Once all options are exhausted, it returns back the list
-- of confirmed solutions up until now.
confirmSolutions :: Int -> [String] -> [[String]] -> IO ()
confirmSolutions _ _ [] = return ()
confirmSolutions n correctOnes (sol:sols) = do
    let guessedWord = head $ filter (not . (`elem` correctOnes)) sol
    putGuess guessedWord correctOnes sol
    hFlush stdout
    answer <- getLine
    if answer == "y"
        then 
            let newCorrectOnes = guessedWord:correctOnes in
            if length newCorrectOnes == n
                then putStrLn "My job here is done! :)"
                else confirmSolutions n newCorrectOnes (filter (elem guessedWord) sols)
        else confirmSolutions n correctOnes (filter (not . elem guessedWord) sols)
