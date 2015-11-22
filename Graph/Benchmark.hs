{-# LANGUAGE ViewPatterns #-}

module Graph.Benchmark where

import Criterion.Main
import Test.QuickCheck
import qualified Graph.Graph as G
import Graph.PSQ
import qualified Data.Vector as V
import Control.Arrow (second)
import qualified Graph.OkasakiRBT as RBT
import qualified Data.Set as S

inf :: G.Weight
inf = maxBound

psqDijkstra :: G.Graph -> V.Vector G.Weight
psqDijkstra g = V.replicate s inf V.// loop (decrease (0,0) psq)
  where
    s = G.size g
    psq = fromOrdList $ zip [0.. (s-1)] (repeat inf)
    decrease (i,w) = adjust (min w) i
    updateQ i w q = let adjs = map (second (+w)) (G.adjacent g i) in foldr decrease q adjs
    loop (minView -> Empty) = []
    loop (minView -> Min b@(i, w) q) = b : loop (updateQ i w q)


graphBenchmarks :: IO [Benchmark]
graphBenchmarks = do
  randomGraph <- generate arbitrary
  randomList <- generate arbitrary :: IO [Int]
  return [ bgroup "graph"
      [
        bgroup "psq" [
          bench "dijkstra" $ nf psqDijkstra randomGraph
        ]
      ],
      bgroup "set" [
        bench "okasaki RBT" $ nf (foldr RBT.insert RBT.empty) randomList,
        bench "containers set" $ nf (foldr S.insert S.empty) randomList
      ]
    ]
