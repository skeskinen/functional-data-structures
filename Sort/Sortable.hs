{-# LANGUAGE FlexibleInstances #-}
module Sort.Sortable where

import Test.QuickCheck
import Control.Monad

type Less a = a -> a -> Bool

class Sortable a where
  new :: Less b -> a b
  add :: b -> a b -> a b
  sort :: a b -> [b]


instance (Sortable a, Ord b, Arbitrary b) => Arbitrary (a b) where
  arbitrary =
    sized $ \n ->
      foldr add (new (<)) <$> replicateM n arbitrary
