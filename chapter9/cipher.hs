module Cipher where 
  import Data.Char

  getShiftedChar :: Int -> Char -> Char
  getShiftedChar shift c = if (isUpper c) then getShiftedCharUpper shift c else getShiftedCharLower shift c

  getShiftedCharLower :: Int -> Char -> Char
  getShiftedCharLower shift c = chr $ (((ord c - ord 'a') + shift) `mod` 26) + ord 'a'

  getShiftedCharUpper :: Int -> Char -> Char
  getShiftedCharUpper shift c = chr $ (((ord c - ord 'A') + shift) `mod` 26) + ord 'A'

  caesar :: Int -> String -> String
  caesar shift s = map (getShiftedChar shift) s

  uncaesar :: Int -> String -> String
  uncaesar shift s = map (getShiftedChar (-shift)) s