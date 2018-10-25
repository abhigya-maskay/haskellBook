module Main where

  class Bifunctor p where

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

  data Deux a b = Deux a b
  instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)
    first f (Deux a b) = Deux (f a) b
    second f (Deux a b) = Deux a (f b)

  data Const a b = Const a
  instance Bifunctor Const where
    bimap f _ (Const a) = Const (f a)
    first f (Const a) = Const (f a)
    second _ (Const a) = Const a

  data Drei a b c = Drei a b c
  instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)
    first f (Drei a b c) = Drei a (f b) c
    second f (Drei a b c) = Drei a b (f c)
 
  data SuperDrei a b c = SuperDrei a b
  instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)
    first f (SuperDrei a b) = SuperDrei a (f b)
    second _ (SuperDrei a b) = SuperDrei a b

  data SemiDrei a b c = SemiDrei a
  instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a
    first _ (SemiDrei a) = SemiDrei a
    second _ (SemiDrei a) = SemiDrei a 

  data Quadriceps a b c d = Quadzzz a b c d
  instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzz a b c d) = Quadzzz a b (f c) (g d)
    first f (Quadzzz a b c d) = Quadzzz a b (f c) d
    second g (Quadzzz a b c d) = Quadzzz a b c (g d)

  data Either2 a b = Left2 a | Right2 b 
  instance Bifunctor Either2 where
    bimap f _ (Left2 a) =  Left2 (f a)
    bimap _ g (Right2 b) = Right2 (g b)
    first f (Left2 a) = Left2 (f a)
    first _ (Right2 b) = Right2 b
    second _ (Left2 a) = Left2 a
    second g (Right2 b) = Right2 (g b)

  main :: IO()
  main = undefined