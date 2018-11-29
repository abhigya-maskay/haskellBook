import Data.Char

filterUpper :: String -> String
filterUpper = filter isUpper

capitalizeFirst :: String -> String
capitalizeFirst s = (toUpper . head $ s) : (tail s)

capitalize :: String -> String
capitalize "" = []
capitalize s = (toUpper . head $ s) : (capitalize . tail $ s)

returnFirstCapital :: String -> Char
returnFirstCapital = toUpper . head