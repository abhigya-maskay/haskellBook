tensDigit :: Integral a => a -> a
tensDigit x = d where
  (xLast, _) = x `divMod` 10
  (_ , d) = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d2 where 
  x2 = x `div` 100
  d2 = x2 `mod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y bool = case bool of
  True -> y
  False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y bool
  | bool == True = y
  | otherwise = x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True = y

g :: (a -> b) -> (a, c) -> (b, c) 
g aToB (a,c) = (aToB a, c)