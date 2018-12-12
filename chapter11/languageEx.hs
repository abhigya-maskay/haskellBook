import           Data.Char
import           Data.List

capitalizeWord :: String -> String
capitalizeWord (x:xs) = toUpper x : xs

capitalizeParagraph :: String -> String
capitalizeParagraph paragraph = (toUpper . head $ paragraph):(capitalize . tail $ paragraph) where
capitalize [] = []
capitalize ('.':[]) = "."
capitalize ('.':xs) = ". " ++ (capitalizeWord . head . words $ xs) ++ capitalize (foldr (++) [] (tail . intersperse " " . words $ xs))
capitalize (x:xs) = x : capitalize xs
