module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Validation e a =
  Failure' e | Success' a deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = (Success' (f a))

instance Monoid e => Applicative (Validation e) where
  pure a = Success' a
  (<*>) (Success' f) (Success' a)  = Success' (f a)
  (<*>) (Success' f) (Failure' e)  = Failure' e
  (<*>) (Failure' e) (Success' a)  = Failure' e
  (<*>) (Failure' e) (Failure' e') = Failure' (e <> e')

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    a <- arbitrary
    e <- arbitrary
    oneof [return $ Failure' e, return $ Success' a]

instance (Eq a, Eq e) => EqProp (Validation e a) where
  (=-=) = eq
type ValidationStringInt = Validation [String] (Int,Int,Int)


main :: IO ()
main = do
  quickBatch $ applicative ((Failure' ["a"]) :: ValidationStringInt)
