import Data.Char

newtype Word' =
  Word' String deriving (Eq, Show)

vowels :: String
vowels = "aeiou"

isVowel :: Char -> Bool
isVowel x = x `elem` vowels

seperateVowelsConsonants :: String -> (String, String)
seperateVowelsConsonants s = ((filter (isVowel) ls),
                              filter (\a -> a `elem` ['a'..'z']) . (filter (not . isVowel)) $ ls)
                             where ls = map toLower s

countEach :: (String, String) -> (Int, Int)
countEach (x1,x2) = (length x1, length x2)

countVowelsConsonants :: String -> (Int, Int)
countVowelsConsonants = countEach . seperateVowelsConsonants

mkWord :: String -> Maybe Word'
mkWord s =
  case (numVowel > numConst) of
    True -> Nothing
    False -> Just (Word' s)
    where numVowel = fst . countVowelsConsonants $ s
          numConst = snd . countVowelsConsonants $ s
