module Main where

import Test.QuickCheck
import Data.Char
import Data.List

twice f = f . f
fourTimes = twice . twice

capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs) = (toUpper x) : xs

genWord :: Gen [Char]
genWord =
  sized $
    \n -> do
      k <- choose (0,n)
      sequence [arbitrary | _ <- [1..k]]

prop_capitalizeWord :: Property
prop_capitalizeWord = forAll genWord (\x ->
  (capitalizeWord x == twice capitalizeWord x)
  &&
  (capitalizeWord x == fourTimes capitalizeWord x))

prop_sort :: Property
prop_sort = forAll genWord (\x ->
                              (sort x == twice sort x)
                              &&
                              (sort x == fourTimes sort x))

main :: IO ()
main = do
  quickCheck prop_capitalizeWord
  quickCheck prop_sort

