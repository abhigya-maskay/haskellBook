lefts' :: [Either a b] -> [a]
lefts' [] = []
lefts' (x:xs) = case x of
  Left a  -> a : lefts' xs
  Right a -> lefts' xs

rights' :: [Either a b] -> [b]
rights' [] = []
rights' (x:xs) = case x of
  Left a  -> rights' xs
  Right a -> a : rights' xs

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' = undefined
