{-# LANGUAGE OverloadedStrings #-}

module Main where
  import Numeric
  import Data.Char
  import Data.List
  import Data.Word
  import Text.Trifecta
  import Data.Maybe
  import Data.List.Split

  data IPAddress = IPAddress Word32 deriving (Eq, Ord)
  instance Show IPAddress where
    show = show . intercalate "." . splitToDecimal . ipToBinary 

  ipToBinary :: IPAddress -> String
  ipToBinary (IPAddress a) = decimalToBinary $ ((fromIntegral a) :: Integer)

  splitToDecimal :: String -> [String]
  splitToDecimal = fmap show . fmap binaryToDecimal . splitPlaces [8,8,8,8]

  leftPad :: Integer -> String -> String
  leftPad n s =  (take (fromIntegral (n - (genericLength s))) $ repeat '0') ++ s

  decimalToBinary :: Integer -> String
  decimalToBinary n = leftPad 8 $ showIntAtBase 2 intToDigit n ""

  binaryToDecimal :: String -> Integer
  binaryToDecimal = fst . fromJust . listToMaybe . readInt 2 (`elem` ("01" :: String)) digitToInt

  binaryToIP :: String -> IPAddress
  binaryToIP s = IPAddress ((fromIntegral . binaryToDecimal $ s) :: Word32)

  ipListToBinary :: [Integer] -> [String]
  ipListToBinary = fmap decimalToBinary

  foldIp :: [Integer] -> String
  foldIp s = foldr (++) [] (ipListToBinary s)
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
    ip <- binaryToIP . foldIp <$> parseIp
    return $ ip

  main :: IO ()
  main = undefined
