import Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe s = Just s

replaceThe :: String -> String
replaceThe = foldr (++) [] . intersperse " " . map replaceString . map notThe . words
  where replaceString Nothing = "a"
        replaceString (Just a) = a

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = countVowel 0 . map notThe . words
  where countVowel :: Integer -> [Maybe String] -> Integer
        countVowel count [] = count
        countVowel count ((Just x1) : xs) = countVowel count xs
        countVowel count (Nothing : ((Just x2):xs)) =
          case ((head x2) `elem` "aeiou") of
            True -> countVowel (count + 1) xs
            False -> countVowel count xs
        countVowel count (_ : xs) = countVowel count xs

testVowel :: Char -> Bool
testVowel x = x `elem` "aeiou"

returnVowels :: String -> String
returnVowels = filter (testVowel)

countVowels :: String -> Int
countVowels = length . returnVowels
