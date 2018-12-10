module Cipher where
import           Data.Char

encode :: String -> String -> String
encode keyword word = shift (getKeywordShifts keyword word) word where
  shift :: [Int] -> [Char] -> [Char]
  shift [] _          = []
  shift _ []          = []
  shift (i:is) (x:xs) = (if (isAlpha x) then (getShiftedChar i x) else x):(shift is xs)

decode :: String -> String -> String
decode keyword word = unshift (getKeywordShifts keyword word) word where
  unshift :: [Int] -> [Char] -> [Char]
  unshift [] _          = []
  unshift _ []          = []
  unshift (i:is) (x:xs) = (if (isAlpha x) then (getShiftedChar (-i) x) else x):(unshift is xs)

getKeywordEncoding :: String -> String -> String
getKeywordEncoding keyword []     = []
getKeywordEncoding keyword (x:xs) = case (isAlpha x) of
  True -> encodeChar keyword : getKeywordEncoding (cycleWord keyword) xs where
    encodeChar :: String -> Char
    encodeChar = head
  False -> x : getKeywordEncoding keyword xs

getShifts :: String -> [Int]
getShifts = map getShift where
  getShift :: Char -> Int
  getShift s = if (isAlpha s) then (ord s - ord 'A') else 0

getKeywordShifts :: String -> String -> [Int]
getKeywordShifts keyword word = getShifts . getKeywordEncoding (map toUpper keyword) $ word

getShiftedChar :: Int -> Char -> Char
getShiftedChar shift c = if (isUpper c) then getShiftedCharUpper shift c else getShiftedCharLower shift c

getShiftedCharUpper :: Int -> Char -> Char
getShiftedCharUpper shift c = chr $ (((ord c - ord 'A') + shift) `mod` 26) + ord 'A'

getShiftedCharLower :: Int -> Char -> Char
getShiftedCharLower shift c = chr $ (((ord c - ord 'a') + shift) `mod` 26) + ord 'a'

cycleWord :: [a] -> [a]
cycleWord s = tail s ++ [head s]
