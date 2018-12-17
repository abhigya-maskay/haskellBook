import Data.Maybe
myIterate :: (a -> a) -> a -> [a]
myIterate f init = init: myIterate f (f init)

myUnfoldr :: (b -> Maybe(a,b)) -> b -> [a]
myUnfoldr f init = fst tuple : (myUnfoldr f . snd $ tuple) where
  tuple = fromJust . f $ init

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr g where
  g x = Just (x, f x)
