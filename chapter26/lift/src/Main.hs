module Main where

instance MonadTrans (EitherT e) where

main :: IO ()
main = do
  putStrLn "hello world"
