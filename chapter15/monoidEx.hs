import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  Trivial <> Trivial = Trivial
instance Monoid Trivial where
  mempty  = Trivial
  mappend = (<>)
instance Arbitrary Trivial where
  arbitrary = return Trivial
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)
instance (Semigroup a ,Monoid a) => Monoid (Identity a) where
  mempty  = Identity mempty
  mappend = (<>)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return $ Identity a
type IdentityAssoc = Identity String -> Identity String -> Identity String -> Bool

data Two a b = Two a b deriving (Eq,Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)
instance (Semigroup a, Monoid a, Semigroup b, Monoid b) => Monoid (Two a b) where
  mempty  = Two mempty mempty
  mappend = (<>)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return $ Two a b
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool

newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup (BoolConj) where
  (BoolConj True)  <> (BoolConj True)  = BoolConj True
  _                <> (BoolConj False) = BoolConj False
  (BoolConj False) <> _                = BoolConj False
instance Monoid (BoolConj) where
  mempty  = BoolConj True
  mappend = (<>)
instance Arbitrary (BoolConj) where
  arbitrary = do
    a <- arbitrary
    return $ BoolConj a
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  _                <> (BoolDisj True)  = BoolDisj True
  (BoolDisj True)  <> _                = BoolDisj True
instance Monoid BoolDisj where
  mempty  = BoolDisj False
  mappend = (<>)
instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return $ BoolDisj a
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

newtype Combine a b = Combine {unCombine :: (a -> b)}
instance Semigroup b => Semigroup (Combine a b) where
  (Combine a) <> (Combine b) = (Combine (a <> b))
instance (Semigroup b, Monoid b) => Monoid (Combine a b) where
  mempty  = (Combine mempty)
  mappend = (<>)

newtype Comp a = Comp (a -> a)
instance Semigroup a => Semigroup (Comp a) where
  (Comp a) <> (Comp b) = Comp (a <> b)
instance (Semigroup a, Monoid a) => Monoid (Comp a) where
  mempty  = Comp mempty
  mappend = (<>)

newtype Mem s a = Mem { runMem :: s -> (a,s)}
instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g =
    Mem (\x -> let (a,b) = g x
                   (c,d) = f b in
                   (a <> c, d))
instance (Semigroup a, Monoid a) => Monoid (Mem s a) where
  mempty  = Mem $ \s -> (mempty, s)
  mappend = (<>)
instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
  arbitrary = do
    a <- arbitrary
    return $ Mem a
type MemAssoc = Mem String Int -> Mem String Int -> Mem String Int -> Bool

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidLeftIdentity a = a <> mempty == a

monoidRightIdentity :: (Eq m, Monoid m, Semigroup m) => m -> Bool
monoidRightIdentity a = mempty <> a == a

main :: IO ()
main = do
  putStrLn "Trivial Monoid"
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (monoidLeftIdentity :: Trivial -> Bool)
  quickCheck (monoidRightIdentity :: Trivial -> Bool)
  putStrLn "Identity Monoid"
  quickCheck (semigroupAssoc :: IdentityAssoc)
  quickCheck (monoidLeftIdentity :: Identity String -> Bool)
  quickCheck (monoidRightIdentity :: Identity String -> Bool)
  putStrLn "Two Monoid"
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (monoidLeftIdentity :: Two String String -> Bool)
  quickCheck (monoidRightIdentity :: Two String String -> Bool)
  putStrLn "BoolConj Monoid"
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
  quickCheck (monoidRightIdentity :: BoolConj -> Bool)
  putStrLn "BoolDisj Monoid"
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (monoidLeftIdentity :: BoolDisj -> Bool)
  quickCheck (monoidRightIdentity :: BoolDisj -> Bool)
  putStrLn "Combine Monoid"
  putStrLn "Not sure how to implement Tests"
  putStrLn "Comp Monoid"
  putStrLn "Not sure how to implement Tests"
  putStrLn "Mem Monoid"
  putStrLn "Not sure how to implement Tests"
