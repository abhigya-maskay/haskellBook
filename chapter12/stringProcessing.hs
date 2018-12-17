import           Data.List

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe a     = Just a

replaceThe :: String -> String
replaceThe = concatMap (replace .notThe ) . intersperse " " . words where
  replace :: Maybe String -> String
  replace Nothing  = "a"
  replace (Just a) = a

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel s = go 0 (words s) where
  go :: Integer -> [String] -> Integer
  go counter [x]  = counter
  go counter (x:xs) = if x == "the" && (head . head $ xs) `elem` "aeiou"
                         then go (counter + 1) xs
                         else go counter xs

checkVowel :: Char -> Bool
checkVowel c = c `elem` "aeiou"

countVowels :: String -> Integer
countVowels = fromIntegral . length . filter checkVowel
