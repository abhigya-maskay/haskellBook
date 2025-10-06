{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Queue where

import Data.List (unsnoc)
import Data.Maybe (fromJust)
import Criterion.Main
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

data Queue a = Queue
  { enqueue :: ![a],
    dequeue :: ![a]
  }
  deriving (Eq, Show, Generic, NFData)

push :: a -> Queue a -> Queue a
push x qx = Queue e d
  where
    e = x : enqueue qx
    d = dequeue qx

pop :: Queue a -> Maybe (a, Queue a)
pop (Queue e d) = case d of
  (x:xs) -> Just (x, Queue e xs)
  [] -> case reverse e of
    [] -> Nothing
    (x:xs) -> Just (x, Queue [] xs)

qNull :: Queue a -> Bool
qNull qx =  null (enqueue qx) && null (dequeue qx)

empty :: Queue a
empty = Queue [] []

newtype Queue' a = Queue' [a] deriving (Eq, Show,Generic, NFData)

push' :: a -> Queue' a -> Queue' a
push' x (Queue' qx) = Queue' $ x:qx

pop' :: Queue' a -> Maybe (a, Queue' a)
pop' (Queue' qx) = process <$> unsnoc qx where
  process :: ([a], a) -> (a, Queue' a)
  process (init', last') = (last', Queue' init')

qNull' :: Queue' a -> Bool
qNull' (Queue' qx) = null qx

empty' :: Queue' a
empty' = Queue' []

pushAll :: [Int] -> Queue Int -> Queue Int
pushAll xs q = foldr push q xs

popAll :: Queue Int -> Queue Int
popAll q = case pop q of
  Nothing -> q
  Just result -> popAll . snd $ result

longChain :: [Int] -> Queue Int
longChain xs = popAll . pushAll xs $ empty 

pushAll' :: [Int] -> Queue' Int -> Queue' Int
pushAll' xs q = foldr push' q xs

popAll' :: Queue' Int -> Queue' Int
popAll' q = case pop' q of
  Nothing -> q
  Just result -> popAll' . snd $ result

longChain' :: [Int] -> Queue' Int
longChain' xs = popAll' . pushAll' xs $ empty' 

alternate :: [Int] -> Queue Int
alternate = foldr (\ x q -> snd . fromJust . pop $ push x q) empty

alternate' :: [Int] -> Queue' Int
alternate' = foldr (\ x q -> snd . fromJust . pop' $ push' x q) empty'


normalUse :: [Int] -> Queue Int
normalUse xs = pushAndPop xs empty where
  pushAndPop [] q = q
  pushAndPop xs' q = pushAndPop (drop 200 xs') (popAll . pushAll (take 200 xs') $ q)
  
normalUse' :: [Int] -> Queue' Int
normalUse' xs = pushAndPop xs empty' where
  pushAndPop [] q = q
  pushAndPop xs' q = pushAndPop (drop 200 xs') (popAll' . pushAll' (take 200 xs') $ q)

main :: IO ()
main = defaultMain
  [
    bench "longChain double q" $
    nf longChain [1..10000],
    bench "longChain single q" $
    nf longChain' [1..10000],
    bench "alternate double q" $
    nf alternate [1..10000],
    bench "alternate single q" $
    nf alternate' [1..10000],
    bench "normal use double q" $
    nf normalUse [1..10000],
    bench "normal use single q" $
    nf normalUse' [1..10000]
  ]

