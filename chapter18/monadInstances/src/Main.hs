import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

type S3 = (String, String, String)

data Nope a = NopeDotJpg deriving (Eq, Show)
instance Functor (Nope) where
  fmap f (NopeDotJpg) = NopeDotJpg
instance Applicative (Nope) where
  pure a = NopeDotJpg
  (<*>) _ _ = NopeDotJpg
instance Monad (Nope) where
  return = pure
  (>>=) _ _ = NopeDotJpg
instance Arbitrary (Nope a) where
  arbitrary = return $ NopeDotJpg
instance EqProp (Nope a) where
  (=-=) = eq
type NopeString = Nope S3

data PhbtEither b a =
  Left' a
  | Right' b deriving (Eq, Show)
instance Functor (PhbtEither b) where
  fmap _ (Right' b) = Right' b
  fmap f (Left' a)  = Left' (f a)
instance Applicative (PhbtEither b) where
  pure a = Left' a
  (<*>) (Right' b) _ = Right' b
  (<*>) _ (Right' b) = Right' b
  (<*>) (Left' f) (Left' a) = Left' (f a)
instance Monad (PhbtEither b) where
  return = pure
  (>>=) (Right' b) _ = Right' b
  (>>=) (Left' a) f  = f a
instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    oneof [return $ Left' a, return $ Right' b]
instance (Eq a, Eq b) => EqProp (PhbtEither b a) where
  (=-=) = eq
type PhbtEitherString = PhbtEither S3 S3

newtype Identity a = Identity a deriving (Eq, Ord, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)
instance Monad Identity where
  return = pure
  (>>=) (Identity a) f = f a
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
instance Eq a => EqProp (Identity a) where
  (=-=) = eq
type IdentityString = Identity S3

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ xs `append` ys

data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)
instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil _ = Nil
  (<*>) _ Nil = Nil
  (<*>) (Cons f fs) xs = (fmap f xs) `append` (fs <*> xs)
instance Monad List where
  return = pure
  (>>=) Nil _ = Nil
  (>>=) (Cons a Nil) f = f a
  (>>=) (Cons a b) f = f a `append` (b >>= f)
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Cons a (Cons b Nil), Nil]
instance (Eq a) => EqProp (List a) where
  (=-=) = eq
type ListString = List S3

main :: IO()
main = do
  putStrLn "Nope: "
  quickBatch $ functor (NopeDotJpg :: NopeString)
  quickBatch $ applicative (NopeDotJpg :: NopeString)
  quickBatch $ monad (NopeDotJpg :: NopeString)
  putStrLn "PhbtEither: "
  quickBatch $ functor (undefined :: PhbtEitherString)
  quickBatch $ applicative (undefined :: PhbtEitherString)
  quickBatch $ monad (undefined :: PhbtEitherString)
  putStrLn "Identity: "
  quickBatch $ functor (undefined :: IdentityString)
  quickBatch $ applicative (undefined :: IdentityString)
  quickBatch $ monad (undefined :: IdentityString)
  putStrLn "List: "
  quickBatch $ functor (undefined :: ListString)
  quickBatch $ applicative (undefined :: ListString)
  quickBatch $ monad (undefined :: ListString)


