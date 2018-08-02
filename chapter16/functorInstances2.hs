{-# LANGUAGE FlexibleInstances#-}
import Test.QuickCheck
import Test.QuickCheck.Function

type IntToInt = Fun Int Int

data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)
instance Functor (Quant a) where
  fmap _ Finance   = Finance
  fmap _ (Desk a)  = Desk a
  fmap f (Bloor b) = Bloor (f b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Quant a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return Finance), (1, return $ Desk a), (1, return $ Bloor b)]
type QuantInt = Quant Int Int
type QuantCompose = Quant Int Int -> IntToInt -> IntToInt -> Bool

data K a b = K a deriving (Eq, Show)
instance Functor (K a) where
  fmap _ (K a) = K a
instance (Arbitrary a, Arbitrary b) => Arbitrary (K a b) where
  arbitrary = do
    a <- arbitrary
    return $ K a
type KInt = K Int Int
type KCompose = K Int Int -> IntToInt -> IntToInt -> Bool

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip (K (f a))
instance (Arbitrary a, Arbitrary b) => Arbitrary(Flip K a b) where
  arbitrary = do
    a <- arbitrary
    return $ Flip (K a)
type FlipKInt = Flip K Int Int
type FlipKCompose = Flip K Int Int -> IntToInt -> IntToInt -> Bool

data EvilGoateeConst a b = GoatyConst b deriving (Eq, Show)
instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst b) = GoatyConst (f b)
instance (Arbitrary b) => Arbitrary (EvilGoateeConst a b) where
  arbitrary = do
    b <- arbitrary
    return $ GoatyConst b
type EvilGoateeConstInt = EvilGoateeConst Int Int
type EvilGoateeConstCompose = EvilGoateeConst Int Int -> IntToInt -> IntToInt -> Bool

data LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut g) = LiftItOut (fmap f g)
instance (Arbitrary (f a)) => Arbitrary (LiftItOut f a) where
  arbitrary = do
    a <- arbitrary
    return $ LiftItOut a

data Parappa f g a = DaWrappa (f a) (g a)
instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap h (DaWrappa f g) = DaWrappa (fmap h f) (fmap h g)


data IgnoreOne f g a b = IgnoringSomething (f a) (g b)
instance Functor g => Functor (IgnoreOne f g a) where
  fmap h (IgnoringSomething f g) = IgnoringSomething f (fmap h g)

data Notorious g o a t =
  Notorious (g o) (g a) (g t)
instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious o a t) = Notorious o a (fmap f t)

data List a = Nil | Cons a (List a)
instance Functor (List) where
  fmap _ Nil = Nil
  fmap f (Cons a b) = Cons (f a) (fmap f b)

data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)
instance Functor (GoatLord) where
  fmap f NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

data TalkToMe a = Halt | Print String a | Read (String -> a)
instance Functor (TalkToMe) where
  fmap _ Halt = Halt
  fmap f (Print s a) = (Print s (f a))
  fmap f (Read s) = Read (fmap f s)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) =>
                  f a
               -> Fun a b
               -> Fun b c
               -> Bool
functorCompose x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)


main :: IO ()
main = do
  putStrLn "Quant Functor"
  quickCheck (functorIdentity :: QuantInt -> Bool)
  quickCheck (functorCompose :: QuantCompose)
  putStrLn "K Functor"
  quickCheck (functorIdentity :: KInt -> Bool)
  quickCheck (functorCompose :: KCompose)
  putStrLn "Flip K functor"
  quickCheck (functorIdentity :: FlipKInt -> Bool)
  quickCheck (functorCompose :: FlipKCompose)
  putStrLn "EvilGoateeConst Functor"
  quickCheck (functorIdentity :: EvilGoateeConstInt -> Bool)
  quickCheck (functorCompose :: EvilGoateeConstCompose)
