module CornellPractice where

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

sumNatural :: Integral a => a -> a
sumNatural 1 = 1
sumNatural n = n + sumNatural (n-1)

myMult :: Integral a => a -> a -> a
myMult a b = go a b 0
  where go a b counter =
          case (b == 0) of
            True -> counter
            False -> go a (b-1) (counter+a)

raiseToPower :: (Eq a, Fractional a,Integral b) => a -> b -> a
raiseToPower a 0 = 1
raiseToPower 0 b = 0
raiseToPower a b =
  case (signum b == 1) of
    True -> r
    False -> 1 / r
  where r =
          case (even b) of
            False -> a * (raiseToPower a $ (abs b)-1)
            True -> raiseToPower (a * a) (div (abs b) 2)

greatestcd :: (Integral a) => a -> a -> a
greatestcd a b =
  case (mod a b == 0) of
    True -> b
    False -> greatestcd b (mod a b)

reverseString :: String -> String
reverseString a = go a []
  where go string collector =
          case (length string == 0) of
            True -> collector
            False -> go (tail string) (concat [[head string], collector])

