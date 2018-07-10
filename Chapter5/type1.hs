i :: a -> a
i x = x

c :: a -> b -> a
c x y = x

c'' :: b -> a -> b
c'' x y = x

c' :: a -> b -> b
c' x y = y

r :: [a] -> [a]
r x = head [x]

co :: (b -> c) -> (a -> b) -> a -> c
co bToC aToB a = bToC(aToB a)

a :: (a -> c) -> a -> a
a cToA a = a

a' :: (a -> b) -> a -> b
a' aToB a = aToB a
