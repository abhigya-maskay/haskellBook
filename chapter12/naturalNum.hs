data Nat =
    Zero
  | Succ Nat
  deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ n) = 1 + (natToInteger n)

integerToNat :: Integer -> Maybe Nat
integerToNat 0 = Just Zero
integerToNat n =
  case (n<0) of
    True -> Nothing
    False -> Just (makeNat n)
    where makeNat 0 = Zero
          makeNat n = Succ(makeNat (n-1))
