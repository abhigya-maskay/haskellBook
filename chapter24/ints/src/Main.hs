{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where
  
  import Data.Text
  import Control.Applicative
  import Text.Trifecta
  import Data.Char

  parseDigit :: Parser Char
  parseDigit = do 
    a <- oneOf ['0'..'9']
    return a

  base10Integer :: Parser Integer
  base10Integer = do
    sign <- optional (char '-')
    num <- some parseDigit
    case sign of 
      Nothing -> return (read num)
      Just x -> return (read $ x:num)

  main :: IO()
  main = undefined