module Main where

import Verba.CLI (runCLI)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= (runCLI . fromMaybe "en" . listToMaybe)
