module Main where

  import Control.Monad.Trans.Except
  import Control.Monad.Trans.Maybe
  import Control.Monad.Trans.Reader

  embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
  embedded = MaybeT . ExceptT . ReaderT $ (const (return (Right (Just 1))))

  main :: IO ()
  main = do
    putStrLn "hello world"
