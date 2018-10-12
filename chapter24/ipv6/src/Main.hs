module Main where

  import Data.Word
  import Numeric
  import Data.Char
  import Data.List
  import Data.Maybe
  import Text.Trifecta
  import Control.Applicative

  data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

  leftPad :: Integer -> String -> String
  leftPad n s = (take (fromIntegral (n - (genericLength s))) $ repeat '0') ++ s

  decimalToBinary :: Integer -> String
  decimalToBinary n = leftPad 16 $ showIntAtBase 2 intToDigit n ""

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

  parseIp :: Parser IPAddress6
  parseIp = do
    a <- binaryToIp . foldAndHalf . addSections . (fmap hexToBinaryTuple) . foldList <$> some parseIpSec
    return a

  main :: IO()
  main = undefined