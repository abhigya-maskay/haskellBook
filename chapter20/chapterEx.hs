import Data.Monoid

data Constant a b = Constant b
instance Foldable (Constant a) where
  foldr f z (Constant b) = f b z
  foldMap f (Constant b) = f b

data Two a b = Two a b
instance Foldable (Two a) where
  foldr f z (Two a b) = f b z
  foldMap f (Two a b) = f b

data Three a b c = Three a b c
instance Foldable (Three a b ) where
  foldr f z (Three a b c) = f c z
  foldMap f (Three a b c) = f c

data Three' a b = Three' a b b
instance Foldable (Three' a) where
  foldr f z (Three' a b b') = f b' z
  foldMap f (Three' a b b') = f b'

data Four' a b = Four' a b b b
instance Foldable (Four' a) where
  foldr f z (Four' a b b' b'') = f b'' z
  foldMap f (Four' a b b' b'') = f b''

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\a -> if (f a) then (pure a) else mempty)

main :: IO()
main = undefined
