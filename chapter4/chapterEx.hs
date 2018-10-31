isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x =  x == reverse x

myAbs :: Integer -> Integer
myAbs x = if x<0 then
    -x else
        x

f1 :: (a,b) -> (c,d) -> ((b,d), (a,c))
f1 ab cd = ((snd ab, snd cd), (fst ab, fst cd))

x = (+)
f2 :: String -> Int
f2 xs = w `x` 1
 where w = length xs

f3 :: (a,b) -> a
f3 (a,b) = a