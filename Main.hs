import Criterion.Main
import Criterion.Types

import Queue.Benchmark
import Sort.Benchmark
import Graph.Benchmark

myConfig = defaultConfig {
    timeLimit = 0.1
  }

main = do
  queue <- queueBenchmarks
  sort <- sortBenchmarks
  graph <- graphBenchmarks
  defaultMainWith myConfig $
    queue ++ sort ++ graph
