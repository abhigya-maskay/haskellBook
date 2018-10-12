{-# LANGUAGE OverloadedStrings #-}

module Main where
  import Numeric
  import Data.Char
  import Data.List
  import Data.Word
  import Text.Trifecta
  import Data.Maybe

  data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

  leftPad :: Integer -> String -> String
  leftPad n s =  (take (fromIntegral (n - (genericLength s))) $ repeat '0') ++ s

  decimalToBinary :: Integer -> String
  decimalToBinary n = leftPad 8 $ showIntAtBase 2 intToDigit n ""

  binaryToDecimal :: String -> IPAddress
  binaryToDecimal = IPAddress . fst . fromJust . listToMaybe . readInt 2 (`elem` ("01" :: String)) digitToInt

  ipToBinary :: [Integer] -> [String]
  ipToBinary = fmap decimalToBinary

  foldIp :: [Integer] -> String
  foldIp s = foldr (++) [] (ipToBinary s)
  foldIP _ = ""

  parseIp :: Parser [Integer]
  parseIp = do
    a <- decimal
    char '.'
    b <- decimal
    char '.'
    c <- decimal
    char '.'
    d <- decimal
    return $ [a,b,c,d]

  parseIpv4 :: Parser IPAddress
  parseIpv4 = do
    ip <- binaryToDecimal . foldIp <$> parseIp
    return $ ip
  main :: IO ()
  main = undefined
