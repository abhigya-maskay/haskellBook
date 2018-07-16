myIterate :: (a -> a) -> a -> [a]
myIterate f s = s  : (myIterate f (f s))

myUnfoldr :: (b -> Maybe (a,b))
          -> b
          -> [a]
myUnfoldr f x = [(fst (x2))] ++ myUnfoldr f (snd (x2))
  where Just x2 = f x

betterIterate :: (a->a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b , (f b)))


