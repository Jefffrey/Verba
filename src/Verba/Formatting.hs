module Verba.Formatting where

blue :: String -> String
blue str = "\o33[1;34m" ++ str ++ "\o33[0m"

green :: String -> String
green str = "\o33[0;32m" ++ str ++ "\o33[0m"

orange :: String -> String
orange str = "\o33[0;33m" ++ str ++ "\o33[0m"
