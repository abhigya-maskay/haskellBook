module Cipher where
import Data.Char

getEncInt :: Int -> Char -> Int
getEncInt n s =
  case (isUpper s) of
    True -> (mod ((ord s) - (ord 'A') + n) 26) + ord 'A'
    False -> (mod ((ord s) - (ord 'a') + n) 26) + ord 'a'

getDecInt :: Int -> Char -> Int
getDecInt n s =
  case (isUpper s) of
    True -> (mod ((ord s) - (ord 'A') - n) 26) + ord 'A'
    False -> (mod ((ord s) - (ord 'a') - n) 26) + ord 'a'

encodeWord :: Int -> String -> String
encodeWord n s =
  case (length s == 0) of
    True -> []
    False -> (chr (getEncInt n . head $ s)) : encodeWord n (tail s)

decodeWord :: Int -> String -> String
decodeWord n s =
  case (length s == 0) of
    True -> []
    False -> (chr (getDecInt n . head $ s)) : decodeWord n (tail s)
