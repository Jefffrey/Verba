module Verba.Formatting where

label :: String -> String
label str = "\o33[2;49;37m" ++ str ++ "\o33[0m"

success :: String -> String
success str = "\o33[0;32m" ++ str ++ "\o33[0m"

warning :: String -> String
warning str = "\o33[0;33m" ++ str ++ "\o33[0m"
