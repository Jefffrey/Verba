module Main where

import Verba.CLI (runCLI)
import Paths_Verba

main :: IO ()
main = getDataFileName "dict/it.dic" >>= runCLI
