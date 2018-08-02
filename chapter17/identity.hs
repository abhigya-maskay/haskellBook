newtype Identity a = Identity a
  deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Applicative Identity where
  pure a = Identity a
  (Identity a) <*> (Identity b) = Identity (a b)
