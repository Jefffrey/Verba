module Verba.Puzzle (Puzzle, consume, ask, fromLists, applyGravity) where

import Data.Matrix (Matrix, (!), setElem, ncols, nrows)
import qualified Data.Matrix as Matrix
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Control.Monad (replicateM)

-- The size of the puzzle needs to be at least 2x2.
newtype Puzzle = Puzzle { getMatrix :: Matrix (Maybe Char) }

instance Show Puzzle where
    show = map (fromMaybe ' ') . intercalate [Just '\n'] . Matrix.toLists . getMatrix

-- Moves the element in the specified position
-- down one cell and puts nothing in its place if the
-- current element has something and the cell below doesn't.
sortWithBelow :: (Int, Int) -> Puzzle -> Puzzle
sortWithBelow current@(i, j) puz@(Puzzle mat) = 
    let below = (i + 1, j) in
    if mat ! current /= Nothing && mat ! below == Nothing then
        Puzzle $ setElem Nothing current $ setElem (mat ! current) below mat
    else puz

-- If the above cell contains something and the below
-- one doesn't, it drops the above cell down
fixColumnCell :: (Int, Int) -> Puzzle -> Puzzle
fixColumnCell (1, _) puz = puz 
fixColumnCell (i, j) puz =
    let calcIx x = (x, j) in
    foldl (flip sortWithBelow) puz (map calcIx $ [1..(i - 1)])

-- Takes a column index and applies gravity
-- to the column from the bottom up. 
fixColumn :: Int -> Puzzle -> Puzzle
fixColumn j puz@(Puzzle mat) = 
    let calcIx i = (i, j) in
    foldr fixColumnCell puz (map calcIx $ [1..nrows mat])

-- Drops a character if the underlying cell
-- is empty.
applyGravity :: Puzzle -> Puzzle
applyGravity puz@(Puzzle mat) = foldr fixColumn puz [1..ncols mat]

-- Function that takes a puzzle and a position
-- and returns all valid neightbours from that position.
getNeightbours :: (Int, Int) -> Puzzle -> [(Int, Int)]
getNeightbours (i, j) (Puzzle puz) =
    let allNeightbours = [(i - 1, j - 1), (i - 1, j), (i - 1, j + 1),
                          (i    , j - 1),             (i    , j + 1),
                          (i + 1, j - 1), (i + 1, j), (i + 1, j + 1)] in
    filter isValid allNeightbours
        where isValid (i, j) = i > 0 && i <= nrows puz && j > 0 && j <= ncols puz

-- Generates a list of all indices in the
-- puzzle matrix.
allPositions :: Puzzle -> [(Int, Int)]
allPositions (Puzzle puz) = 
    let (r, c) = (nrows puz, ncols puz) in
    map (fn c) [0..(r * c) - 1]
        where fn nc i = ((i `div` nc) + 1, (i `mod` nc) + 1)

-- Consumes a character in the specified location
-- and drops the characters above that one.
consumeChar :: (Int, Int) -> Puzzle -> Puzzle
consumeChar ix (Puzzle puz) = Puzzle $ setElem Nothing ix puz

-- Tries to consume the string starting at the 
-- specified position.
consumeAt :: String -> Puzzle -> (Int, Int) -> [Puzzle]
consumeAt (ch : rest) (Puzzle puz) ix =
    if (puz ! ix) == (Just ch) then 
        let newPuz = consumeChar ix (Puzzle puz)
            neightbours = getNeightbours ix (Puzzle puz) in
        if rest /= [] then
            concatMap (consumeAt rest newPuz) neightbours
        else [newPuz]
    else []

-- Tries to consume the string and returns
-- a list of resulting puzzles from the consumption
-- of said string.
consume :: String -> Puzzle -> [Puzzle]
consume str puz = map applyGravity $ concatMap (consumeAt str puz) (allPositions puz)

-- Asks for a matrix in the form of 
-- rows of strings.
ask :: IO Puzzle
ask = do
    hd <- getLine
    tl <- replicateM (length hd - 1) getLine
    return . Puzzle . Matrix.fromLists . map (map Just) $ hd : tl

-- Constructs a puzzle from a list of list
-- of characters.o
fromLists = Puzzle . Matrix.fromLists
