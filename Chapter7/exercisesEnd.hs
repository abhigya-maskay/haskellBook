tensDigit :: Integral a => a -> a
tensDigit x = d
  where
    (xLast, _) = x `divMod` 10
    (_, d) = xLast `divMod` 10

hunsD :: Integral a => a -> a
hunsD x = d
  where
    (xLast, _) = x `divMod` 100
    (_, d) = xLast `divMod` 10

foldBool :: a -> a -> Bool -> a
foldBool x y b =
  case b of
    True -> y
    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
  | b == True = y
  | otherwise = x

g :: (a->b) -> (a,c) -> (b,c)
g aToB (a,c) = ((aToB a),c)

