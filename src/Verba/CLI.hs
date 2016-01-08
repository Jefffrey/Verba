module Verba.CLI (runCLI) where

import Data.List (intersperse, intercalate)
import System.IO (hFlush, stdout)
import Verba.Solver (solve)
import Verba.Dictionary (Dictionary)
import Verba.Puzzle (Puzzle)
import qualified Verba.Dictionary as Dictionary
import qualified Verba.Puzzle as Puzzle

runCLI :: FilePath -> IO ()
runCLI dictFile = do
    putStrLn "Puzzle:"
    puz <- Puzzle.ask
    putStr "Word lengths (separated by space): "
    hFlush stdout
    lengths <- getLine >>= (return . map read . words)
    dict <- Dictionary.load (maximum lengths) dictFile
    let sols = solve dict lengths puz
    confirmSolutions (length lengths) [] sols 

-- Checks if the first list is contained in the
-- second list.
supersetOf :: (Eq a) => [a] -> [a] -> Bool
supersetOf set maybeSs = all (\x -> x `elem` maybeSs) set

green :: String -> String
green str = "\o33[0;32m" ++ str ++ "\o33[0m"

orange :: String -> String
orange str = "\o33[0;33m" ++ str ++ "\o33[0m"

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
