{-# LANGUAGE TupleSections #-}
module Graph.Graph where

import Test.QuickCheck

import qualified Data.Vector as V
import Control.Monad (mapM)

type Index = Int
type Weight = Int
type Edge = (Index, Weight)

data Graph = Graph (V.Vector [Edge])
  deriving Show

instance Arbitrary Graph where
  arbitrary =
    let edges n i = do
          xs <- sublistOf $ [0 .. i-1] ++ [i+1 .. n]
          mapM (\i -> (i,) <$> choose (1,100)) xs
      in sized $ \n ->
        Graph <$> V.generateM n (edges n)

adjacent :: Graph -> Index -> [Edge]
adjacent (Graph vertices) i = vertices V.! i

size :: Graph -> Int
size (Graph v) = length v
