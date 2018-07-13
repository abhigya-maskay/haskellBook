module Cipher where
import Data.Char

getEncInt :: Int -> Char -> Int
getEncInt _ ' ' = 32
getEncInt n s =
  case (isUpper s) of
    True -> (mod ((ord s) - (ord 'A') + n) 26) + ord 'A'
    False -> (mod ((ord s) - (ord 'a') + n) 26) + ord 'a'

getDecInt :: Int -> Char -> Int
getDecInt _ ' ' = 32
getDecInt n s =
  case (isUpper s) of
    True -> (mod ((ord s) - (ord 'A') - n) 26) + ord 'A'
    False -> (mod ((ord s) - (ord 'a') - n) 26) + ord 'a'

getKeywordEncoding :: String -> String
getKeywordEncoding " " = " "
getKeywordEncoding s = go s "ALLY" []
  where go string "" acc = go string "ALLY" acc
        go "" _ acc = acc
        go string ally acc =
          case (head string) of
            ' ' -> go (tail string) ally (acc ++ " ")
            _   -> go (tail string) (tail ally) (acc ++ [head ally])

getShift :: Char -> Int
getShift ' ' = 0
getShift s   =
  case (isUpper s) of
    True -> (ord s) - (ord 'A')
    False -> (ord s) - (ord 'a')

encodeWord :: String -> String
encodeWord s = go s (getKeywordEncoding s)
  where go string key =
          case (length string == 0) of
            True -> []
            False -> (chr (getEncInt (getShift . head $ key) . head $ string)) : (go (tail string) (tail key))

decodeWord :: Int -> String -> String
decodeWord n s =
  case (length s == 0) of
    True -> []
    False -> (chr (getDecInt n . head $ s)) : decodeWord n (tail s)

