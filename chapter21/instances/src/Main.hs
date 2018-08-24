module Main where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
instance (Eq a) => EqProp (Identity a) where
  (=-=) = eq
instance Functor Identity where
  fmap f (Identity a) = Identity $ f a
instance Applicative Identity where
  pure = Identity
  (<*>) (Identity f) (Identity x) = Identity (f x)
instance Foldable Identity where
  foldMap f (Identity x) = f x
instance Traversable Identity where
  traverse f (Identity x) = fmap Identity (f x)
type S3 = (String, String, String)
type IdentityString = Identity S3

newtype Constant a b = Constant { getConstant :: a}
  deriving (Eq, Show)
instance Arbitrary a => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return $ Constant a
instance (Eq a) => EqProp (Constant a b) where
  (=-=) = eq
instance Functor (Constant a) where
  fmap f (Constant a) = Constant a
instance Monoid a => Applicative (Constant a) where
  pure a = Constant mempty
  (<*>) (Constant f) (Constant a) = Constant (f <> a)
instance Foldable (Constant a) where
  foldMap _ _ = mempty
instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a
type ConstantString = Constant S3 S3

data Optional a = Nada | Yep a deriving (Eq, Show)
instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    oneof [(return $ Nada), (return $ Yep a)]
instance Eq a => EqProp (Optional a) where
  (=-=) = eq
instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Yep a) = Yep (f a)
instance Applicative Optional where
  pure = Yep
  (<*>) _ Nada = Nada
  (<*>) Nada _ = Nada
  (<*>) (Yep f) (Yep a) = Yep (f a)
instance Foldable Optional where
  foldMap f Nada = mempty
  foldMap f (Yep a) = f a
instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Yep a) = fmap Yep (f a)
type OptionalString = Optional S3

data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = genList
genList :: Arbitrary a => Gen (List a)
genList = do
  a <- arbitrary
  b <- genList
  frequency [(3, return $ Cons a b), (1, return Nil)]
instance Eq a => EqProp (List a) where
  (=-=) = eq
instance Semigroup (List a) where
  (<>) x Nil = x
  (<>) Nil x = x
  (<>) (Cons x xs) ys = Cons x (xs <> ys)
instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)
instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f g) xs = (fmap f xs) <> (g <*> xs)
instance Foldable List where
  foldMap _ Nil = mempty
  foldMap f (Cons x xs) = (f x) <> foldMap f xs
instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
type ListString = List S3

data Three a b c = Three a b c deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure c = Three mempty mempty c
  (<*>) (Three f g h) (Three a b c) = Three (f <> a) (g <> b) (h c)
instance Foldable (Three a b) where
  foldMap f (Three a b c) = f c
instance Traversable (Three a b) where
  traverse f (Three a b c) = fmap (Three a b) (f c)
type ThreeString = Three S3 S3 S3

data Pair a b = Pair a b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Pair a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b
instance (Eq a, Eq b) => EqProp (Pair a b) where
  (=-=) = eq
instance Functor (Pair a) where
  fmap f (Pair a b) = Pair a (f b)
instance Monoid a => Applicative (Pair a) where
  pure b = Pair mempty b
  (<*>) (Pair f g) (Pair a b) = Pair (f <> a) (g b)
instance Foldable (Pair a) where
  foldMap f (Pair a b) = f b
instance Traversable (Pair a) where
  traverse f (Pair a b) = fmap (Pair a) (f b)
type PairString = Pair S3 S3

data Big a b = Big a b b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    b' <- arbitrary
    return $ Big a b b'
instance (Eq a, Eq b) => EqProp(Big a b) where
  (=-=) = eq
instance Functor (Big a) where
  fmap f (Big a b b') = Big a (f b) (f b')
instance Monoid a => Applicative (Big a) where
  pure b = Big mempty b b
  (<*>) (Big f g g') (Big a b b') = Big (f <> a) (g b) (g' b')
instance Foldable (Big a) where
  foldMap f (Big a b b') = (f b) <> (f b')
instance Traversable (Big a) where
  traverse f (Big a b b') = (Big a) <$> (f b) <*> (f b')
type BigString = Big S3 S3

data Bigger a b = Bigger a b b b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Bigger a b) where
  arbitrary = do
    a   <- arbitrary
    b   <- arbitrary
    b'  <- arbitrary
    b'' <- arbitrary
    return $ Bigger a b b' b''
instance (Eq a, Eq b) => EqProp(Bigger a b) where
  (=-=) = eq
instance Functor (Bigger a) where
  fmap f (Bigger a b b' b'') = Bigger a (f b) (f b') (f b'')
instance Monoid a => Applicative (Bigger a) where
  pure b = Bigger mempty b b b
  (<*>) (Bigger f g g' g'') (Bigger a b b' b'') = Bigger (f <> a) (g b) (g' b') (g'' b'')
instance Foldable (Bigger a) where
  foldMap f (Bigger a b b' b'') = (f b) <> (f b') <> (f b'')
instance Traversable (Bigger a) where
  traverse f (Bigger a b b' b'') = (Bigger a) <$> (f b) <*> (f b') <*> (f b'')
type BiggerString = Bigger S3 S3

main :: IO ()
main = do
  putStrLn "Identity"
  quickBatch $ functor (undefined :: IdentityString)
  quickBatch $ applicative (undefined :: IdentityString)
  quickBatch $ traversable (undefined :: IdentityString)
  putStrLn "Constant"
  quickBatch $ functor (undefined :: ConstantString)
  quickBatch $ applicative (undefined :: ConstantString)
  quickBatch $ traversable (undefined :: ConstantString)
  putStrLn "Optional"
  quickBatch $ functor (undefined :: OptionalString)
  quickBatch $ applicative (undefined :: OptionalString)
  quickBatch $ traversable (undefined :: OptionalString)
  putStrLn "List"
  quickBatch $ functor (undefined :: ListString)
  quickBatch $ applicative (undefined :: ListString)
  quickBatch $ traversable (undefined :: ListString)
  putStrLn "Three"
  quickBatch $ functor (undefined :: ThreeString)
  quickBatch $ applicative (undefined :: ThreeString)
  quickBatch $ traversable (undefined :: ThreeString)
  putStrLn "Pair"
  quickBatch $ functor (undefined :: PairString)
  quickBatch $ applicative (undefined :: PairString)
  quickBatch $ traversable (undefined :: PairString)
  putStrLn "Big"
  quickBatch $ functor (undefined :: BigString)
  quickBatch $ applicative (undefined :: BigString)
  quickBatch $ traversable (undefined :: BigString)
  putStrLn "Bigger"
  quickBatch $ functor (undefined :: BiggerString)
  quickBatch $ applicative (undefined :: BiggerString)
  quickBatch $ traversable (undefined :: BiggerString)
