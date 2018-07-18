import Control.Monad
import System.Exit
import Data.Char

makeLowerCase :: String -> String
makeLowerCase = map toLower

filterLetters :: String -> String
filterLetters = filter (\s -> s `elem` ['a'..'z'])

palindromeCheck :: String -> Bool
palindromeCheck x = y == (reverse y)
  where y = (filterLetters . makeLowerCase $ x)

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  case (palindromeCheck line1) of
    True -> putStrLn "It's a palindrome!"
    False -> do
      putStrLn "Nope!"
      exitSuccess
