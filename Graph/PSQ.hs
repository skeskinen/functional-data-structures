{-# LANGUAGE ViewPatterns, MultiParamTypeClasses #-}
module Graph.PSQ where

import Data.List (sortOn)
import Prelude hiding (lookup)

data PSQ k p = Void | Winner (k, p) (Tree k p) k
  deriving Show
data Tree k p = Leaf | Node Int (k, p) (Tree k p) k (Tree k p)
  deriving Show

node b l k r = Node (1 + size l + size r) b l k r

size :: Tree p k -> Int
size Leaf = 0
size (Node s _ _ _ _) = s

data MinView k p = Empty | Min (k, p) (PSQ k p)
  deriving Show

minView :: (Ord k, Ord p) => PSQ k p -> MinView k p
minView Void = Empty
minView (Winner b t m) = Min b (secondBest t m)

data TourView k p = Null | Single (k, p) | PSQ k p :# PSQ k p
tourView :: (Ord k, Ord p) => PSQ k p -> TourView k p
tourView Void = Null
tourView (Winner b Leaf m) = Single b
tourView (Winner b (Node _ b' t k u) m)
  | key b' <= k = Winner b' t k :# Winner b u m
  | otherwise = Winner b t k :# Winner b' u m

foldm :: (a -> a -> a) -> a -> [a] -> a
foldm _ e [] = e
foldm f e as = fst (go (length as) as)
  where
    go 1 (a:as) = (a, as)
    go n as = let
      m = n `div` 2
      (a1, as1) = go (n-m) as
      (a2, as2) = go m as1
      in (f a1 a2, as2)

prio :: (k, p) -> p
prio (_, p) = p

key :: (k, p) -> k
key (k, _) = k

maxKey :: PSQ k p -> k
maxKey Void = error "empty PSQ has no max key"
maxKey (Winner _ _ m) = m

singleton :: (k, p) -> PSQ k p
singleton b = Winner b Leaf (key b)

fromList :: (Ord k, Ord p) => [(k, p)] -> PSQ k p
fromList = fromOrdList . sortOn key

fromOrdList :: (Ord p) => [(k,p)] -> PSQ k p
fromOrdList = foldm (#) Void . map singleton

(#) :: (Ord p) => PSQ k p -> PSQ k p -> PSQ k p
Void # b = b
a # Void = a
Winner b t m # Winner b' t' m'
  | prio b < prio b' = Winner b (node b' t m t') m'
  | otherwise = Winner b' (node b t m t') m'

secondBest :: (Ord k, Ord p) => Tree k p -> k -> PSQ k p
secondBest Leaf m = Void
secondBest (Node _ b t k u) m
  | key b <= k = Winner b t k # secondBest u m
  | otherwise = secondBest t k # Winner b u m

deleteMin :: (Ord k, Ord p) => PSQ k p -> PSQ k p
deleteMin (Winner _ t m) = secondBest t m

toOrdList :: (Ord k, Ord p) => PSQ k p -> [(k, p)]
toOrdList (tourView -> Null) = []
toOrdList (tourView -> Single b) = [b]
toOrdList (tourView -> t :# u) = toOrdList t ++ toOrdList u

lookup :: (Ord k, Ord p) => k -> PSQ k p -> Maybe p
lookup k (Winner (k', p) _ _)
  | k == k' = Just p
lookup _ (tourView -> Null) = Nothing
lookup k (tourView -> Single (k', p))
  | k == k' = Just p
  | otherwise = Nothing
lookup k (tourView -> t :# u)
  | maxKey t <= k = lookup k t
  | otherwise = lookup k u

adjust :: (Ord k, Ord p) => (p -> p) -> k -> PSQ k p -> PSQ k p
adjust _ _ Void = Void
adjust f k t@(tourView -> Single b)
  | k == key b = singleton (k, f $ prio b)
  | otherwise = t
adjust f k (tourView -> t :# u)
  | k <= maxKey t = adjust f k t # u
  | otherwise = t # adjust f k u

insert :: (Ord k, Ord p) => (k, p) -> PSQ k p -> PSQ k p
insert b Void = singleton b
insert b t@(tourView -> Single b')
  | key b < key b' = singleton b # t
  | key b == key b' = singleton b
  | otherwise = t # singleton b
insert b (tourView -> t :# u)
  | key b <= maxKey t = insert b t # u
  | otherwise = t # insert b u

delete :: (Ord k, Ord p) => k -> PSQ k p -> PSQ k p
delete k Void = Void
delete k t@(tourView -> Single b)
  | k == key b = Void
  | otherwise = t
delete k (tourView -> t :# u)
  | k <= maxKey t = delete k t # u
  | otherwise = t # delete k u

delta,ratio :: Int
delta = 3
ratio = 2

balance b l k r
  | size l + size r < 2 = node b l k r
  | size r > delta * size l = balanceL b l k r
  | size l > delta * size r = balanceR b l k r
  | otherwise = node b l k r

balanceL b l k r@(Node _ _ rl _ rr)
  | size rl < ratio * size rr = singleL b l k r
  | otherwise                 = doubleL b l k r

balanceR b l@(Node _ _ ll _ lr) k r
  | size lr < ratio * size ll = singleL b l k r
  | otherwise                 = doubleL b l k r

singleL, singleR, doubleL, doubleR :: (Ord k, Ord p) => (k, p) -> Tree k p -> k -> Tree k p -> Tree k p
singleR b (Node _ lb ll lk lr) k r
  | key lb <= lk && prio b <= prio lb = node b  ll lk (node lb lr k r)
  | otherwise                         = node lb ll lk (node b  lr k r)

singleL b l k (Node _ rb rl rk rr)
  | key rb <= rk && prio b <= prio rb = node b  (node rb l k rl) rk rr
  | otherwise                         = node rb (node b  l k rl) rk rr

doubleL b l k (Node _ rb rl rk rr) = singleL b l k (singleR rb rl rk rr)
doubleR b (Node _ lb ll lk lr) = singleR b (singleL lb ll lk lr)
