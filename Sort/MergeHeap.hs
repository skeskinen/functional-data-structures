module Sort.MergeHeap where

import qualified Sort.Heap as H

data Heap a = Empty | Heap a (H.Heap (Heap a))
  deriving Eq

instance Ord a => Ord (Heap a) where
  compare Empty a = error "empty compare"
  compare a Empty = error "empty compare"
  compare (Heap x _) (Heap y _) = compare x y

merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty a = a
merge a Empty = a
merge h1@(Heap x p1) h2@(Heap y p2)
  | x < y = Heap x (H.insert h2 p1)
  | otherwise = Heap y (H.insert h1 p2)

deleteMin :: Ord a => Heap a -> Heap a
deleteMin (Heap _ p) = let
  (Heap x p1) = H.findMin p
  p2 = H.deleteMin p
  in Heap x (H.merge p2 p1)

findMin :: Heap a -> a
findMin (Heap x _ ) = x
