module Verba.Dictionary where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import System.IO

type Dictionary = Map Int [String]

-- Pushes a word in the right bucket
-- based on its length.
insertWord :: String -> Dictionary -> Dictionary
insertWord w = Map.alter fn (length w)
    where fn Nothing = Just []
          fn (Just lst) = Just $ w : lst

-- Loads the dictionary so that only words up to
-- the specified length are loaded.
load :: Int -> FilePath -> IO Dictionary
load n file = withFile file ReadMode $ (\handle -> do
    hSetEncoding handle latin1
    contents <- hGetContents handle
    let wordList = takeWhile ((<= n) . length) $ words contents
    return $! foldr insertWord Map.empty wordList)

-- Gets all the words with a certain length.
wordsWithLength :: Int -> Dictionary -> [String]
wordsWithLength n = fromMaybe [] . Map.lookup n
