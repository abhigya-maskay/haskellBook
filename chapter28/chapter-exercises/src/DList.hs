module DList where

import Criterion.Main

newtype DList a = DL {unDL :: [a] -> [a]}

empty :: DList a
empty = DL id
{-# INLINE empty #-}

singleton :: a -> DList a
singleton a = DL (a :)
{-# INLINE singleton #-}

toList :: DList a -> [a]
toList = ($ []) . unDL
{-# INLINE toList #-}

infixr 9 `cons`

cons :: a -> DList a -> DList a
cons x xs = DL $ (x :) . unDL xs
{-# INLINE cons #-}

infixr 9 `snoc`

snoc :: DList a -> a -> DList a
snoc xs x = DL $ unDL xs . (x :)
{-# INLINE snoc #-}

append :: DList a -> DList a -> DList a
append xs xs' = DL $ unDL xs . unDL xs'
{-# INLINE append #-}

schlemiel :: Int -> [Int]
schlemiel i = go i []
  where
    go 0 xs = xs
    go n xs = go (n - 1) (xs ++ [n])

constructDList :: Int -> [Int]
constructDList i = toList $ go i empty
  where
    go 0 xs = xs
    go n xs = go (n - 1) (xs `append` singleton n)

main :: IO ()
main =
  defaultMain
    [ bench "concat list" $
        nf schlemiel 12345,
      bench "concat dlist" $
        nf constructDList 12345
    ]
