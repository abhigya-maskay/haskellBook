module Main where

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance Functor m => Functor (EitherT e m) where
  fmap f (EitherT ma) = EitherT $ (fmap . fmap) f ma

instance Applicative m => Applicative (EitherT e m) where
  pure x = EitherT $ pure (pure x)

  (EitherT fab) <*> (EitherT mma) = EitherT $ (<*>) <$> fab <*> mma

instance Monad m => Monad (EitherT e m) where
  return = pure

  (EitherT ma) >>= f = EitherT $ do
    v <- ma
    case v of
      Left x -> return $ Left x
      Right y -> runEitherT $ f y

swapEither :: Either e a -> Either a e
swapEither a = case a of
  Left x -> Right x
  Right x -> Left x

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT ma) = EitherT $ swapEither <$> ma

either :: (a -> c) -> (b -> c) -> Either a b -> c
either fac _ (Left a) = fac a
either _ fbc (Right b) = fbc b

eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT famc fbmc (EitherT x) = do
  a <- x
  Main.either famc fbmc a

main :: IO ()
main = do
  putStrLn "hello world"
