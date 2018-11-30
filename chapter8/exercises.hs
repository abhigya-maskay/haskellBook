mySum :: (Eq a, Num a) => a -> a
mySum 1 = 1
mySum n = n + mySum (n-1)

myMult :: Integral a => a -> a -> a
myMult x 1 = x
myMult x n = x + myMult x (n-1)

data DividedResult = Result Integer | DividedByZero deriving Show

dividedBy :: Integral a => a -> a -> DividedResult
dividedBy n d
  | d == 0 = DividedByZero
  | signum(n) == signum(d) = Result(r)
  | otherwise = Result(-r)
  where
    r = go (abs n) (abs d) 0
    go n d count =
      case n<d of
        True -> count
        _ -> go (n-d) d (count+1)

mc91 :: (Ord a, Num a) => a -> a
mc91 a = if a>100 then a-10 else mc91 $ mc91 $ a+11
