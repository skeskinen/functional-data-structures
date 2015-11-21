{-# LANGUAGE RankNTypes #-}
module Sort.Benchmark where

import Criterion.Main
import Test.QuickCheck
import Control.Monad
import qualified Sort.Merge as Merge
import Sort.Sortable

testData :: forall a. Arbitrary a => IO a
testData = generate . resize 10000 $ arbitrary

sortBenchmarks :: IO [Benchmark]
sortBenchmarks =
  return . bgroup "sort" <$> sequence [
      bench "merge" . nf sort <$> (testData :: IO (Merge.Sort Int))
    ]
