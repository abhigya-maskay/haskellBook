module Main where

import Test.QuickCheck

data Fool =
    Fulse
  | Frue
  deriving (Eq, Show)

genEqual :: Gen Fool
genEqual = oneof [return Fulse, return Frue]

genUnequal :: Gen Fool
genUnequal = frequency [(2,return Fulse), (1,return Frue)]

main :: IO ()
main = do
  putStrLn "Equal"
  sample genEqual
  putStrLn "2/3 and 1/3"
  sample genUnequal
