module Main where

import Test.QuickCheck
import Data.List (sort)
import TestFunctions

prop_half :: Double -> Bool
prop_half x = ((*2) . half) x == x

prop_listOrdered :: [Int] -> Bool
prop_listOrdered xs =
  (snd $ foldr go (Nothing, True) (sort xs))
    where go _ status@(_,False) = status
          go y (Nothing, t)     = (Just y, t)
          go y (Just x, t)      = (Just y, x >= y)

prop_plusAssociative :: Integer -> Integer -> Integer -> Bool
prop_plusAssociative x y z =
  x + (y + z) == (x + y) + z

prop_plusCommutative :: Integer -> Integer -> Bool
prop_plusCommutative x y =
  x + y == y + x

prop_multAssociative :: Integer -> Integer -> Integer -> Bool
prop_multAssociative x y z = x * (y * z) == (x * y) * z

prop_multCommutative :: Integer -> Integer -> Bool
prop_multCommutative x y = x * y == y * x

genDivTuple :: Gen (Integer, Integer)
genDivTuple = do
  a <- arbitrary
  b <- arbitrary `suchThat` (/=0)
  return (a,b)

myQuotRem :: (Integer,Integer) -> Bool
myQuotRem (x,y) = (quot x y) * y + (rem x y) == x

myDivMod :: (Integer,Integer) -> Bool
myDivMod (x,y) = (div x y)*y + (mod x y) == x

prop_quotRem :: Property
prop_quotRem =
  forAll genDivTuple myQuotRem

prop_divMod :: Property
prop_divMod =
  forAll genDivTuple myDivMod

prop_exponentCommutative :: Integer -> Integer -> Bool
prop_exponentCommutative x y = x^y == y^x

prop_exponentAssociative :: Integer -> Integer -> Integer -> Bool
prop_exponentAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z

prop_list :: [Integer] -> Bool
prop_list x = reverse (reverse x) == id x

prop_dollar :: ([Integer] -> [Integer]) -> [Integer] -> Bool
prop_dollar f x = (f $ x) == f x

prop_period :: ([Integer] -> [Integer]) -> ([Integer] -> [Integer]) -> [Integer] -> Bool
prop_period f g x = (f . g) x == f (g x)

prop_listFoldr :: [Integer] -> Bool
prop_listFoldr x = foldr (:) [] x == ((++) [] x)

prop_listFoldr2 :: [Integer] -> Bool
prop_listFoldr2 x = foldr (++) [] [x] == (concat [x])

genTakeTup :: Gen ([Integer],Int)
genTakeTup = do
  b <- arbitrary `suchThat` (>0)
  a <- arbitrary `suchThat` (\x -> (length x > b))
  return (a,b)

myTake :: ([Integer], Int) -> Bool
myTake (xs,n) = length (take n xs) == n

prop_take :: [Integer] -> Property
prop_take xs = forAll genTakeTup myTake

prop_read :: Integer -> Bool
prop_read x = (read (show x)) == x

main :: IO ()
main = do
  quickCheck prop_half
  quickCheck prop_listOrdered
  quickCheck prop_plusAssociative
  quickCheck prop_plusCommutative
  quickCheck prop_multCommutative
  quickCheck prop_multAssociative
  quickCheck prop_quotRem
  quickCheck prop_divMod
  quickCheck prop_exponentCommutative
  quickCheck prop_exponentAssociative
  quickCheck prop_list
  quickCheck (prop_dollar reverse)
  quickCheck (prop_period reverse (drop 3))
  quickCheck prop_listFoldr
  quickCheck prop_listFoldr2
  quickCheck prop_take
  quickCheck prop_read
