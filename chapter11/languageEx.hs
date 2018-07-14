import Data.Char
import Data.List

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph s = foldr (++) [] ((intersperse " ") . go . words $ s)
  where
    go [] = []
    go xs@(x:xtail) =
          case ('.' `elem` x) of
            True ->  x : ((capitalizeWord . head $ xtail) : (go . tail $ xtail))
            False -> (x : (go xtail))
