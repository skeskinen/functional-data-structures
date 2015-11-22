{-# LANGUAGE FlexibleInstances #-}
module Sort.Sortable where

type Less a = a -> a -> Bool

class Sortable a where
  new :: Less b -> a b
  add :: b -> a b -> a b
  sort :: a b -> [b]
