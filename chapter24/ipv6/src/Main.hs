module Main where

  import Data.Word
  import Numeric
  import Data.Char
  import Data.List
  import Data.Maybe
  import Text.Trifecta
  import Control.Applicative
  import Data.List.Split
  import Control.Monad
  import Control.Arrow

  data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord)
  instance Show IPAddress6 where
    show =  show . intercalate ":" . createIpString . hexIpList where 
      hexIpList = splitToHex . ipToBinary

  createIpString :: [String] -> [String]
  createIpString s = go s False [] where
    maxNum = findMax s
    isMaxOccurence xs = (length . takeWhile (== "0") $ xs) == maxNum
    go [] doubleColon acc = reverse acc
    go ("0":[]) False acc = reverse (":" : acc )
    go ("0":xs) False acc = 
      if (isMaxOccurence ("0":xs)) then
        go (dropWhile (== "0") (xs)) True (":" : acc) else
          go xs False ("0" : acc)
    go (x:xs) doubleColon acc = go xs doubleColon (x:acc)

  ipToBinary :: IPAddress6 -> String
  ipToBinary (IPAddress6 a b) = (binary a ++ binary b) where
    binary :: Word64 -> String
    binary w = leftPad 64 . decimalToBinary $ ((fromIntegral w) :: Integer)

  splitToHex :: String -> [String]
  splitToHex = capitalizeLetters . fmap decimalToHex . fmap binaryToDecimal <$> splitPlaces (take 8 $ repeat 16) 

  findMax :: [String] -> Int
  findMax s = go s 0 0 where
    go ("0":[]) acc prev = max acc (prev+1)
    go (x:[]) acc prev = max acc prev
    go ("0":xs) acc prev = go xs acc (prev + 1)
    go (x:xs) acc prev = go xs (max acc prev) 0

  leftPad :: Integer -> String -> String
  leftPad n s = (take (fromIntegral (n - (genericLength s))) $ repeat '0') ++ s

  decimalToBinary :: Integer -> String
  decimalToBinary n = leftPad 16 $ showIntAtBase 2 intToDigit n ""

  binaryToDecimal :: String -> Integer
  binaryToDecimal = fst . fromJust . listToMaybe . readInt 2 (`elem` "01") digitToInt

  decimalToHex :: Integer -> String
  decimalToHex n = showIntAtBase 16 intToDigit n ""

  capitalizeLetters :: [String] -> [String]
  capitalizeLetters s = fmap capitalize <$> s where
    capitalize :: Char -> Char
    capitalize s = if (isAlpha s) then
      toUpper s else
        s

  hexToDecimal :: String -> Integer
  hexToDecimal = fst . fromJust . listToMaybe . readHex 

  hexToBinaryTuple :: (String, String) -> (String, String)
  hexToBinaryTuple t = (decimalToBinary . hexToDecimal . fst $ t, snd t)

  binaryToIp :: (String, String) -> IPAddress6
  binaryToIp s = IPAddress6 (stringToWord . fst $ s) (stringToWord . snd $ s) where
    stringToWord = fst . fromJust . listToMaybe . readInt 2 (`elem` ("01" :: String)) digitToInt

  parseIpSec :: Parser [(String, String)]
  parseIpSec = do
    a <- option "" $ some (char ':')
    b <- some alphaNum
    c <- option "" $ some (char ':')
    if a == "" then
      return $ [(b,c)] else
        return $ [("0",a), (b,c)]
  
  foldList :: [[a]] -> [a]
  foldList = foldr (++) []

  foldAndHalf :: [String] -> (String, String)
  foldAndHalf s =  splitAt (((length folded) + 1) `div` 2) $ folded where
    folded = foldList s

  addSections :: [(String, String)] -> [String]
  addSections s = checkSection <$> s where
    missing = 8 - (genericLength s)
    checkSection :: (String, String) -> String
    checkSection p = if (snd p == ":" || snd p == "") then
      fst p else
        fst p ++ (take (missing * 16) $ repeat '0')

  parseIpv6 :: Parser IPAddress6
  parseIpv6 = do
    a <- binaryToIp . foldAndHalf . addSections . (fmap hexToBinaryTuple) . foldList <$> some parseIpSec
    return a

  main :: IO()
  main = undefined