module Verba.Dictionary where

import Data.Map (Map)
import Data.Maybe (fromMaybe)
import qualified Data.Map as Map
import System.IO
import Paths_Verba
import Data.Foldable (foldrM)

type Dictionary = Map Int [String]

-- Pushes a word in the right bucket
-- based on its length.
insertWord :: String -> Dictionary -> Dictionary
insertWord w = Map.alter fn (length w)
    where fn Nothing = Just [w]
          fn (Just lst) = Just $ w : lst

empty :: Dictionary
empty = Map.empty

-- Takes a string representing the name of
-- the language ("it" for "Italian", "en" for "English")
-- and the length of the words you want to load and loads
-- those words in the dictionary.
load :: String -> Int -> Dictionary -> IO Dictionary
load lang len dict = do
    fileName <- getDataFileName $ "dict/" ++ lang ++ "-" ++ show len ++ ".dict"
    words <- readFile fileName >>= (return . words)
    return $ Map.insert len words dict

loadAll :: String -> [Int] -> IO Dictionary
loadAll lang lns = foldrM (load lang) empty lns

-- Gets all the words with a certain length.
wordsWithLength :: Int -> Dictionary -> [String]
wordsWithLength n = fromMaybe [] . Map.lookup n
