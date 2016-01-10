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

runCLI :: String -> IO ()
runCLI lang = do
    putStrLn . label . unlines $
        [ "What's the matrix row by"
        , "row without spaces?"
        ]
    puz <- Puzzle.ask
    putStr "\n"
    putStrLn . label . unlines $
        [ "What's the length of the"
        , "words separated by space?"
        ]
    lengths <- getLine >>= (return . map read . words)
    putStr "\n"
    putStrLn . label . unlines $ 
        [ "Do you know some of the"
        , "words already?"
        ]
    knownWords <- getLine >>= (return . words)
    putStr "\n"
    dict <- Dictionary.loadAll lang lengths
    let sols = solve dict lengths puz
    putStrLn . label . unlines $ 
        [ "Answer 'y' or press enter to"
        , "the following questions."
        ]
    let initFilter = filter (supersetOf knownWords) sols
    confirmSolutions (length lengths) knownWords initFilter

putGuess :: String -> [String] -> [String] -> IO ()
putGuess guessedWord correctOnes guess = do
    let styledWords = map (\w -> if (w `elem` correctOnes)
            then success w
            else if w == guessedWord
                then warning w
                else w) guess
    putStr $ label "Is it " ++ (intercalate " " styledWords) ++ label "? "

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
                then putStrLn . label $ "\nSeems like we are done."
                else confirmSolutions n newCorrectOnes (filter (elem guessedWord) (sol:sols))
        else confirmSolutions n correctOnes (filter (not . elem guessedWord) sols)
