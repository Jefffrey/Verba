module Verba.Utils where

-- Checks if the first list is contained in the
-- second list.
supersetOf :: (Eq a) => [a] -> [a] -> Bool
supersetOf set maybeSs = all (\x -> x `elem` maybeSs) set
