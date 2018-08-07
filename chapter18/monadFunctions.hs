j :: Monad m => m (m a) -> m a
--j a = a >>= id
j a = do
  x <- a
  x

l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = do
  x <- a
  return $ f x

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = do
  a' <- a
  b' <- b
  return $ f a' b'

a :: Monad m => m a -> m (a -> b) -> m b
a a f = do
  a' <- a
  f' <- f
  return $ f' a'

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] f = return []
meh (x:xs) f = do
  a <- f x
  a' <- meh xs f
  return $ a:a'

flipType :: (Monad m) => [m a] -> m [a]
flipType [] = return []
flipType xs = meh xs id

main :: IO()
main = undefined
