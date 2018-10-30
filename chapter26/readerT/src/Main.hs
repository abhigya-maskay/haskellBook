module Main where

newtype ReaderT r m a = ReaderT {runReaderT :: r -> m a}

instance Functor m => Functor (ReaderT r m) where
  fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance Applicative m => Applicative (ReaderT r m) where

  pure x = ReaderT $ pure (pure x)

  (ReaderT fmab) <*> (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance Monad m => Monad (ReaderT r m) where
  return = pure
  (ReaderT rma ) >>= f =
    ReaderT $ \r -> do
      a <- rma r
      runReaderT (f a) r

main :: IO ()
main = do
  putStrLn "hello world"
