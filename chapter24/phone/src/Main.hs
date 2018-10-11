{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where
  import Data.Text hiding (count)
  import Control.Applicative
  import Text.Trifecta

  type NumberingPlanArea = Integer
  type Exchange = Integer
  type LineNumber = Integer

  data PhoneNumber = 
    PhoneNumber NumberingPlanArea 
    Exchange LineNumber
    deriving (Eq, Show)

  parsePhone :: Parser PhoneNumber
  parsePhone = do
    optional (string "1")
    a <- skipMany (oneOf "-() ") >> read <$> count 3 digit
    b <- skipMany (oneOf "-() ") >> read <$> count 3 digit
    c <- skipMany (oneOf "-() ") >> read <$> count 4 digit
    return $ PhoneNumber a b c

  main :: IO()
  main = undefined