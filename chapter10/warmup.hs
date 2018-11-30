stops = "pbtdkg"
vowels = "aeiou"

makeWords :: String -> String -> [String]
makeWords stops vowels = [ [a,b,c] | a <- stops, b <- vowels, c <- stops]

makeWordsFiltered :: String -> String -> [String]
makeWordsFiltered stops vowels = [ [a,b,c] | a <- stops, b<- vowels, c <- stops, a == 'p']