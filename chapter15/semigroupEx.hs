import Data.Semigroup
import Test.QuickCheck

data Trivial = Trivial deriving (Eq, Show)
instance Semigroup Trivial where
  _ <> _ = Trivial
instance Arbitrary Trivial where
  arbitrary = return Trivial
type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


newtype Identity a = Identity a deriving (Eq, Show)
instance Semigroup a => Semigroup (Identity a) where
  (Identity x) <> (Identity y) = Identity (x <> y)
instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    x <- arbitrary
    return $ Identity x
type IdentityStringAssoc = Identity String -> Identity String -> Identity String -> Bool


data Two a b = Two a b deriving (Eq, Show)
instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two x1 y1) <> (Two x2 y2) = Two (x1 <> x2) (y1 <> y2)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    return $ Two x y
type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool


data Three a b c = Three a b c deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  (Three x y z) <> (Three x' y' z') = Three (x <> x') (y <> y') (z <> z')
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Three x y z
type ThreeStringAssoc = Three String String String
                      -> Three String String String
                      -> Three String String String
                      -> Bool


data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four w x y z <> Four w' x' y' z' = Four (w <> w') (x <> x') (y <> y') (z <> z')
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
  arbitrary = do
    w <- arbitrary
    x <- arbitrary
    y <- arbitrary
    z <- arbitrary
    return $ Four w x y z
type FourStringAssoc = Four String String String String
                     -> Four String String String String
                     -> Four String String String String
                     -> Bool


newtype BoolConj = BoolConj Bool deriving (Eq, Show)
instance Semigroup BoolConj where
  (BoolConj True)  <> (BoolConj True)  = BoolConj True
  (BoolConj False) <> _                = BoolConj False
  _                <> (BoolConj False) = BoolConj False
instance Arbitrary BoolConj where
  arbitrary = do
    x <- arbitrary
    return $ BoolConj x
type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)
instance Semigroup BoolDisj where
  (BoolDisj False) <> (BoolDisj False) = BoolDisj False
  (BoolDisj True)  <> _                = BoolDisj True
  _                <> (BoolDisj True)  = BoolDisj True
instance Arbitrary BoolDisj where
  arbitrary = do
    x <- arbitrary
    return $ BoolDisj x
type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool


data Or a b = Fst a | Snd b deriving (Eq, Show)
instance Semigroup (Or a b) where
  (Fst x) <> (Snd y) = (Snd y)
  (Snd x) <> (Fst y) = (Snd x)
  (Snd x) <> (Snd y) = (Snd x)
  (Fst x) <> (Fst y) = (Fst y)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ Fst x), (1, return $ Snd y)]
type OrStringAssoc = Or String String -> Or String String -> Or String String -> Bool

newtype Combine a b = Combine {unCombine :: (a -> b)} 
instance Semigroup b => Semigroup (Combine a b) where
  (Combine f) <> (Combine g) = Combine (f <> g)

newtype Comp a = Comp { unComp :: (a -> a) }
instance Semigroup a => Semigroup (Comp a) where
  (Comp x) <> (Comp y) = Comp (x <> y)

data Validation a b = MyFailure a | MySuccess b
  deriving (Eq, Show)
instance Semigroup a => Semigroup (Validation a b) where
  (MySuccess x) <> (MySuccess y) =MySuccess x
  (MySuccess x) <> (MyFailure y) =MySuccess x
  (MyFailure x) <> (MySuccess y) =MySuccess y
  (MyFailure x) <> (MyFailure y) =MyFailure (x <> y)
instance (Arbitrary x, Arbitrary y) => Arbitrary (Validation x y) where
  arbitrary = do
    x <- arbitrary
    y <- arbitrary
    frequency [(1, return $ MySuccess x), (1, return $ MyFailure y)]
type ValidationAssoc = Validation String Int -> Validation String Int -> Validation String Int -> Bool

main :: IO ()
main = do
  quickCheck (semigroupAssoc :: TrivAssoc)
  quickCheck (semigroupAssoc :: IdentityStringAssoc)
  quickCheck (semigroupAssoc :: TwoAssoc)
  quickCheck (semigroupAssoc :: ThreeStringAssoc)
  quickCheck (semigroupAssoc :: FourStringAssoc)
  quickCheck (semigroupAssoc :: BoolConjAssoc)
  quickCheck (semigroupAssoc :: BoolDisjAssoc)
  quickCheck (semigroupAssoc :: OrStringAssoc)
  quickCheck (semigroupAssoc :: ValidationAssoc)
