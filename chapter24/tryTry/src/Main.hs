{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
import Data.Ratio ((%))
import Text.Trifecta

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  return (numerator % denominator)

parseDecimal :: Parser Double
parseDecimal = do
  dec <- double
  return dec

parseEither :: Parser (Either Rational Double)
parseEither =
  Left <$> try parseFraction
  <|> Right <$> parseDecimal

main :: IO ()
main = do
  print $ parseString parseEither mempty "1/2"
  print $ parseString parseEither mempty "2/1"
  print $ parseString parseEither mempty "1.4"
  print $ parseString parseEither mempty "2"
