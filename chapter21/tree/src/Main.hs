module Main where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data Tree a = Empty | Leaf a | Node (Tree a) a (Tree a)
  deriving (Eq, Show)
instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = genTree
genTree :: Arbitrary a => Gen (Tree a)
genTree = do
  a <- arbitrary
  b <- genTree
  c <- genTree
  frequency [(1, return $ Empty), (2, return $ Leaf a), (1, return $ Node b a c)]
instance (Eq a) => EqProp(Tree a) where
  (=-=) = eq
instance Functor Tree where
  fmap f Empty = Empty
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node t b t') = Node (fmap f t) (f b) (fmap f t')
instance Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Leaf a) = f a
  foldMap f (Node t b t') = (foldMap f t) <> (f b) <> (foldMap f t')
instance Traversable Tree where
  traverse f Empty = pure Empty
  traverse f (Leaf a) = Leaf <$> (f a)
  traverse f (Node t b t') = Node <$> (traverse f t) <*> (f b) <*> (traverse f t')
type S3 = (String, String, String)
type TreeString = Tree S3

main :: IO ()
main = do
  quickBatch $ functor (undefined :: TreeString)
  quickBatch $ traversable (undefined :: TreeString)
