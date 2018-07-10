stops = "pbtdkg"
vowels = "aeiou"

combineStopVowel :: [String]
combineStopVowel = [[x] ++ [y] ++ [z] | x <- stops, y <- vowels, z <- stops]

combineStopVowelP :: [String]
combineStopVowelP = filter (\x -> head x == 'p') combineStopVowel

combineStopVowelTup :: [(Char,Char,Char)]
combineStopVowelTup = [(x,y,z) | x <- stops, y <- vowels, z <- stops]

seekritFunc :: String -> Double
seekritFunc x =
  (/) (fromIntegral $ sum (map length (words x))) (fromIntegral $ length (words x))
