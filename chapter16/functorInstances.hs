import Test.QuickCheck
import Test.QuickCheck.Function

type IntToInt = Fun Int Int

newtype Identity a = Identity a deriving (Eq, Show)
instance Functor Identity where
  fmap f (Identity a) = Identity (f a)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
type IdentityInt = Identity Int
type IdentityCompose = IdentityInt -> IntToInt -> IntToInt -> Bool


data Pair a = Pair a a deriving (Eq, Show)
instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)
instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return $ Pair a a
type PairInt = Pair Int
type PairCompose = Pair Int -> IntToInt -> IntToInt -> Bool


data Two a b = Two a b deriving (Eq, Show)
instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)
instance (Arbitrary a, Arbitrary b) => Arbitrary(Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
type TwoInt = Two Int Int
type TwoCompose = Two Int Int -> IntToInt -> IntToInt -> Bool

data Three a b c = Three a b c deriving (Eq, Show)
instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary(Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ Three a b c
type ThreeInt = Three Int Int Int
type ThreeCompose = Three Int Int Int -> IntToInt -> IntToInt -> Bool

data Three' a b = Three' a b b deriving (Eq, Show)
instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)
instance (Arbitrary a, Arbitrary b) => Arbitrary(Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Three' a b b
type Three'Int = Three' Int Int
type Three'Compose = Three' Int Int -> IntToInt -> IntToInt -> Bool

data Four a b c d = Four a b c d deriving (Eq, Show)
instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return $ Four a b c d
type FourInt = Four Int Int Int Int
type FourCompose = Four Int Int Int Int -> IntToInt -> IntToInt -> Bool

data Four' a b = Four' a a a b deriving (Eq, Show)
instance Functor (Four' a) where
  fmap f (Four' a a' a'' b) = Four' a a' a'' (f b)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Four' a a a b
type Four'Int = Four' Int Int
type Four'Compose = Four' Int Int -> IntToInt -> IntToInt -> Bool

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
  putStrLn "Identity Functor"
  quickCheck (functorIdentity :: IdentityInt -> Bool)
  quickCheck (functorCompose :: IdentityCompose)
  putStrLn "Pair Functor"
  quickCheck (functorIdentity :: PairInt -> Bool)
  quickCheck (functorCompose :: PairCompose)
  putStrLn "Two Functor"
  quickCheck (functorIdentity :: TwoInt -> Bool)
  quickCheck (functorCompose :: TwoCompose)
  putStrLn "Three Functor"
  quickCheck (functorIdentity :: ThreeInt -> Bool)
  quickCheck (functorCompose :: ThreeCompose)
  putStrLn "Three' Functor"
  quickCheck (functorIdentity :: Three'Int -> Bool)
  quickCheck (functorCompose :: Three'Compose)
  putStrLn "Four Functor"
  quickCheck (functorIdentity :: FourInt -> Bool)
  quickCheck (functorCompose :: FourCompose)
  putStrLn "Four' Functor"
  quickCheck (functorIdentity :: Four'Int -> Bool)
  quickCheck (functorCompose :: Four'Compose)
