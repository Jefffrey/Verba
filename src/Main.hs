module Main where

import Verba.Input
import Verba.Dictionary (Dictionary)
import qualified Verba.Dictionary as Dictionary

main :: IO ()
main = Dictionary.load 2 "dict/it.dic" >>= print
