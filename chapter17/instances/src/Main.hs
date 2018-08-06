module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Pair a = Pair a a deriving (Eq, Show)
instance Semigroup a => Semigroup (Pair a) where
  (<>) (Pair a a') (Pair b b') = Pair (a <> b) (a' <> b')
instance Monoid a => Monoid (Pair a) where
  mempty = Pair mempty mempty
  mappend = (<>)
instance Functor Pair where
  fmap f (Pair a a') = Pair (f a) (f a')
instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Pair a b
instance Eq a => EqProp (Pair a) where
  (=-=) = eq
type PairIntStringChar = Pair (Integer,String,Char)

data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (<>) (Two a b) (Two a' b') = Two (a <> a') (b <> b')
instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
instance Monoid a => Applicative (Two a) where
  pure b = Two mempty b
  (<*>) (Two a f) (Two a' b) = Two (a <> a') (f b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
instance (Eq a, Eq b) => EqProp (Two a b) where
  (=-=) = eq
type TwoString = Two (String, String, String) (String, String, String)

data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (<>) (Three a b c) (Three a' b' c') = Three (a <> a') (b <> b') (c <> c')
instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three mempty mempty mempty
  mappend = (<>)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure a = Three mempty mempty a
  (<*>) (Three a b f) (Three a' b' c) = Three (a <> a') (b <> b') (f c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
  (=-=) = eq
type S3 = (String, String, String)
type ThreeString = Three S3 S3 S3

data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  (<>) (Four a b c d) (Four a' b' c' d') = Four (a <> a') (b <> b') (c <> c') (d <> d')
instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty  = Four mempty mempty mempty mempty
  mappend = (<>)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure d = Four mempty mempty mempty d
  (<*>) (Four a b c f) (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d
instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where
  (=-=) = eq
type FourString = Four S3 S3 S3 S3

data Four' a b = Four' a a a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Four' a b) where
  (<>) (Four' a a' a'' b) (Four' c c' c'' d) = Four' (a <> c) (a' <> c') (a'' <> c'') (b <> d)
instance (Monoid a, Monoid b) => Monoid (Four' a b) where
  mempty  = Four' mempty mempty mempty mempty
  mappend = (<>)
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)
instance (Monoid a) => Applicative (Four' a) where
  pure b = Four' mempty mempty mempty b
  (<*>) (Four' a a' a'' f) (Four' c c' c'' d) = Four' (a <> c) (a' <> c') (a'' <> c'') (f d)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a   <- arbitrary
    a'  <- arbitrary
    a'' <- arbitrary
    b   <- arbitrary
    return $ Four' a a' a'' b
instance (Eq a, Eq b) => EqProp (Four' a b) where
  (=-=) = eq
type Four'String = Four' S3 S3

main :: IO ()
main = do
  putStrLn "PAIR: "
  quickBatch $ semigroup (Pair "h")
  quickBatch $ monoid (Pair "h")
  quickBatch $ functor (undefined :: PairIntStringChar)
  quickBatch $ applicative (undefined :: PairIntStringChar)
  putStrLn "TWO: "
  quickBatch $ semigroup (Two "a" "b")
  quickBatch $ monoid (Two "a" "b")
  quickBatch $ functor (undefined :: TwoString)
  quickBatch $ applicative (undefined :: TwoString)
  putStrLn "THREE: "
  quickBatch $ semigroup (Three "a" "b" "c")
  quickBatch $ monoid (Three "a" "b" "c")
  quickBatch $ functor (undefined :: ThreeString)
  quickBatch $ applicative (undefined :: ThreeString)
  putStrLn "FOUR: "
  quickBatch $ semigroup (Four "a" "b" "c" "d")
  quickBatch $ monoid (Four "a" "b" "c" "d")
  quickBatch $ functor (undefined :: FourString)
  quickBatch $ applicative (undefined :: FourString)
  putStrLn "FOUR': "
  quickBatch $ semigroup (Four' "a" "b" "c" "d")
  quickBatch $ monoid (Four' "a" "b" "c" "d")
  quickBatch $ functor (undefined :: Four'String)
  quickBatch $ applicative (undefined :: Four'String)
