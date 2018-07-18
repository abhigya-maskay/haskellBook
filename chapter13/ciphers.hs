module Cipher where
import Data.Char

getEncInt :: Int -> Char -> Int
getEncInt n s =
  case (isUpper s) of
    True  -> (mod ((ord s) - (ord 'A') + n) 26) + ord 'A'
    False -> (mod ((ord s) - (ord 'a') + n) 26) + ord 'a'

getDecInt :: Int -> Char -> Int
getDecInt n s =
  case (isUpper s) of
    True  -> (mod ((ord s) - (ord 'A') - n) 26) + ord 'A'
    False -> (mod ((ord s) - (ord 'a') - n) 26) + ord 'a'

encodeWordCaesar     :: Int -> String -> String
encodeWordCaesar n s =
  case (length s == 0) of
    True  -> []
    False -> (chr (getEncInt n . head $ s)) : encodeWordCaesar n (tail s)

decodeWordCaesar     :: Int -> String -> String
decodeWordCaesar n s =
  case (length s == 0) of
    True  -> []
    False -> (chr (getDecInt n . head $ s)) : decodeWordCaesar n (tail s)

getKeywordEncoding :: String -> String -> String
getKeywordEncoding key word = go key word []
  where go _ "" acc = acc
        go "" w acc = go key w acc
        go k w acc = go (tail k) (tail w) (acc ++ [head k])

getShift     :: Char -> Int
getShift ' ' = 0
getShift s   =
  case (isUpper s) of
    True  -> (ord s) - (ord 'A')
    False -> (ord s) - (ord 'a')

encodeWordVignere   :: String -> String -> String
encodeWordVignere x s = go s (getKeywordEncoding x s)
  where go string key =
          case (length string == 0) of
            True  -> []
            False -> (chr (getEncInt (getShift . head $ key) . head $ string)) : (go (tail string) (tail key))

decodeWordVignere     :: String -> String -> String
decodeWordVignere x s = go s (getKeywordEncoding x s)
  where go string key =
          case (length string == 0) of
            True  -> []
            False -> (chr (getDecInt (getShift . head $ key) . head $ string)) : (go (tail string) (tail key))

caesarEncode :: IO String
caesarEncode = do
  putStrLn "Enter the string you want to encode: "
  word   <- getLine
  putStrLn "Enter the shift: "
  number <- getLine
  return $ encodeWordCaesar (read number :: Int) word

caesarDecode :: IO String
caesarDecode = do
  putStrLn "Enter the string you want to decode: "
  word   <- getLine
  putStrLn "Enter the shift: "
  number <- getLine
  return $ decodeWordCaesar (read number :: Int) word

vignereEncode :: IO String
vignereEncode = do
  putStrLn "Enter the string you want to encode: "
  word <- getLine
  putStrLn "Enter the encoding word: "
  key <- getLine
  return $  encodeWordVignere key word

vignereDecode :: IO String
vignereDecode = do
  putStrLn "Enter the string you want to decode: "
  word <- getLine
  putStrLn "Enter the decoding word: "
  key <- getLine
  return $ decodeWordVignere key word
