module Queue.Benchmark where

import Criterion.Main
import qualified Queue.Lazy as L
import qualified Queue.Mutable as M
import qualified Queue.Log as Lg
import qualified Queue.RealTime as R
import Queue.Util
import Test.QuickCheck
import Control.Monad
import Data.Maybe

moreSnoc :: [QueueInstruction] -> Bool
moreSnoc xs = isJust $ foldl (\m a -> case a of
  Snoc _ -> succ <$> m
  Tail -> (((>0) <$> m) >>= guard) *> (pred <$> m)
  Head -> (((>0) <$> m) >>= guard) *> m
  ) (Just 0) xs


queueBenchmarks :: IO [Benchmark]
queueBenchmarks = do
  randomQueueShort <- generate . resize 10000 $ arbitrary `suchThat` moreSnoc
  randomQueueLong <- generate . resize 500000 $ arbitrary `suchThat` moreSnoc
  return [
    bgroup "queue" $
      map (\(desc, sample) ->
          bgroup desc
            [ bench "lazy" $ nf L.eval sample
            , bench "mutable" $ nf M.eval sample
            , bench "lg worst case" $ nf Lg.eval sample
            , bench "real time" $ nf R.eval sample
            ]
        ) [("short", randomQueueShort), ("long", randomQueueLong)]
    ]
