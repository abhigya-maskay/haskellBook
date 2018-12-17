newtype Word' = Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

checkVowels :: Char -> Bool
checkVowels c = c `elem` vowels

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter checkVowels

countConsonants :: String -> Integer
countConsonants = fromIntegral . length . filter (not . checkVowels)

mkWord :: String -> Maybe Word'
mkWord s = if countVowels s < countConsonants s
              then Just (Word' s)
              else Nothing
