import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

data List a =
  Nil
  | Cons a (List a)
  deriving (Eq, Show)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)
instance Applicative List where
  pure a = Cons a Nil
  (<*>) _ Nil = Nil
  (<*>) Nil _ = Nil
  (<*>) (Cons f fs) xs = (fmap f xs) `append` (fs <*> xs)
instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    elements [Cons a (Cons b Nil), Nil]

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys =
  Cons x $ xs `append` ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons a as) = go n (Cons a as) 0 (Nil)
  where
    go n (Cons a Nil) count acc = (Cons a Nil)
    go n (Cons a as) count acc =
          case (n == count) of
            True -> acc
            False -> go n as (count + 1) (acc `append` (pure a))

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)
instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                    in take' 3000 l
          ys' = let (ZipList' l) = ys
                    in take' 3000 l
instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs
instance Applicative ZipList' where
  pure a = ZipList' (pure a)
  (<*>) _ (ZipList' Nil) = ZipList' Nil
  (<*>) (ZipList' Nil) _ = ZipList' Nil
  (<*>) (ZipList' (Cons f Nil)) (ZipList' as) = ZipList' $ (f <$> as)
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons a Nil)) = ZipList' $ (Cons (f a) (fs <*> pure a))
  (<*>) (ZipList' (Cons f fs)) (ZipList' (Cons a as)) =
    ZipList' $ (Cons (f a) (fsas)) where
    ZipList' fsas = (ZipList' fs) <*> (ZipList' as)
instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = do
    a <- arbitrary
    return $ ZipList' a
intStringChar = Cons ((1, "H", 'c') :: (Int, String, Char)) Nil

main :: IO()
main = do
  quickBatch $ applicative (ZipList' intStringChar)
