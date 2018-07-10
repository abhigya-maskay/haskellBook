filterMult :: Int -> [Int]
filterMult n = filter (\x -> rem x n == 0) [1..30]

filterMultLength :: Int -> Int
filterMultLength n = length . (filter (\x -> rem x n == 0)) $ [1..30]

getWords :: String -> [String]
getWords sent =
      case (' ' `elem` sent) of
        True -> [takeWhile (/= ' ') sent] ++ getWords (tail $ dropWhile(/= ' ') sent)
        False -> [sent]
myFilter :: String -> [String]
myFilter s = filter (\x -> not (x `elem` ["a","an","the"])) $ getWords s
