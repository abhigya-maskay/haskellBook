import Data.Monoid

sum :: (Foldable t, Num a) => t a -> a
sum = getSum . foldMap Sum

product :: (Foldable t, Num a) => t a -> a
product = getProduct. foldMap Product

elem :: (Foldable t, Eq a) => a -> t a -> Bool
elem x = foldr (\a b -> (a == x) || b) False

minimum :: (Foldable t, Ord a) => t a -> Maybe a
minimum = (foldr maybeMin Nothing)
  where maybeMin x (Just y) = Just (if (x>y) then y else x)
        maybeMin x _ = Just x

maximum :: (Foldable t, Ord a) => t a -> Maybe a
maximum = (foldr maybeMax Nothing)
  where maybeMax x (Just y) = Just (if (x>y) then x else y)
        maybeMax x _ = Just x

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> True) False

length :: (Foldable t) => t a -> Int
length = foldr (\a b -> b + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (\a b -> a:b) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap (<> mempty)

foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\a b -> (f a) <> b) mempty

main :: IO()
main = undefined
