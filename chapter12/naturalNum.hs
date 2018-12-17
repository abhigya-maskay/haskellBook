data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero     = 0
natToInteger (Succ a) = 1 + natToInteger a

integerToNat :: Integer -> Maybe Nat
integerToNat i = if i < 0
                    then Nothing
                    else Just (naturalize i) where
                      naturalize :: Integer -> Nat
                      naturalize 0 = Zero
                      naturalize i = Succ (naturalize (i-1))

