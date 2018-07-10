import Data.Char

returnUpper :: String -> String
returnUpper s = filter (isUpper) s

capitalizeFirst :: String -> String
capitalizeFirst s = toUpper (head s) : (tail s)

capitalize :: String -> String
capitalize s =
  case (length s == 0) of
    True -> []
    False -> toUpper (head s) : (capitalize $ tail s)

firstCapital :: String -> Char
firstCapital s = (toUpper . head) s
