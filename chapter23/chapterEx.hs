{-# LANGUAGE InstanceSigs #-}

newtype Moi s a = Moi {runMoi :: s -> (a,s)}

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ (\s -> let (a,s') = g s in (f a, s'))

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ (\s -> (a,s))
  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (Moi f) <*> (Moi g) = Moi $ (\s -> let (a, s') = f s
                                         (b, s'') = g s' in (a b, s''))

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (Moi f) >>= g = Moi $ (\s -> let (a, s') = f s
                                   ms = runMoi $ g a
                                   in ms s')

get :: Moi s s
get = Moi $ \s -> (s,s)

put :: s -> Moi s ()
put a = Moi $ \s -> ((), a)

exec :: Moi s a -> s -> s
exec (Moi sa) s = let (_,s) = sa s in s

eval :: Moi s a -> s -> a
eval (Moi sa) s = let (v,_) = sa s in v

modify :: (s -> s) -> Moi s ()
modify f = Moi $ (\s -> let x= f s in ((),x))

main :: IO()
main = undefined
