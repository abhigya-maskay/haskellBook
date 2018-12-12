import           Data.Char

isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] _ = True
isSubseqOf _ [] = False
isSubseqOf subseq@(s:ss) (x:xs) = case (s == x) of
  True  -> isSubseqOf ss xs
  False -> isSubseqOf subseq xs

capitalizeWords :: String -> [(String, String)]
capitalizeWords = map capitalize . words where
  capitalize :: String -> (String, String)
  capitalize word@(x:xs) = (word, toUpper x : xs)
