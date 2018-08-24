{-# LANGUAGE InstanceSigs #-}

import Control.Monad

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader $ (\r -> a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r (ra r))

instance Monad (Reader r) where
  return = pure
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (Reader ra) >>= aRb = join $ Reader $ \r -> aRb (ra r)

getDogRM :: Person -> Dog
getDogRM = do
  name <- dogName
  addy <- address
  return $ Dog name addy

getDogRM' :: Reader Person Dog
getDogRM' = Dog <$> Reader dogName <*> Reader address

newtype HumanName = HumanName String deriving (Eq, Show)
newtype DogName = DogName String deriving (Eq, Show)
newtype Address = Address String deriving (Eq, Show)

data Person = Person {
      humanName :: HumanName
    , dogName :: DogName
    , address :: Address
    } deriving (Eq, Show)

data Dog = Dog {
      dogsName :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)



main :: IO()
main = undefined
