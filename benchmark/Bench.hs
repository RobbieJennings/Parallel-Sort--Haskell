module Main where

import Criterion.Main
import System.Random

import Mergesort
import Quicksort

hundred = (take 100 (randomRs (0,100) (mkStdGen 42)))::[Int]
thousand = (take 1000 (randomRs (0,100) (mkStdGen 42)))::[Int]
tenthousand = (take 10000 (randomRs (0,100) (mkStdGen 42)))::[Int]

main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks = [ bgroup "Benchmarks"
               [ quicksortBenchmark
               , mergesortBenchmark
               ]
             ]


-------BENCHMARKING QUICKSORT--------

quicksortBenchmark :: Benchmark
quicksortBenchmark
 = bgroup "Quicksort"
    [ serialQuicksortBenchmark
    , parallelQuicksortBenchmark
    ]

serialQuicksortBenchmark :: Benchmark
serialQuicksortBenchmark
 = bgroup "Serial"
    [ bench "100"
        ( whnf quicksortSerial hundred )
    , bench "1000"
        ( whnf quicksortSerial thousand )
    , bench "10000"
        ( whnf quicksortSerial tenthousand )
    ]

parallelQuicksortBenchmark :: Benchmark
parallelQuicksortBenchmark
 = bgroup "Parallel"
     [ bench "100"
         ( whnf quicksortParallel hundred )
     , bench "1000"
         ( whnf quicksortParallel thousand )
     , bench "10000"
         ( whnf quicksortParallel tenthousand )
     ]


-------BENCHMARKING MERGESORT--------

mergesortBenchmark :: Benchmark
mergesortBenchmark
 = bgroup "Mergesort"
    [ serialMergesortBenchmark
    , parallelMergesortBenchmark
    ]

serialMergesortBenchmark :: Benchmark
serialMergesortBenchmark
 = bgroup "Serial"
     [ bench "100"
         ( whnf mergesortSerial hundred )
     , bench "1000"
         ( whnf mergesortSerial thousand )
     , bench "10000"
         ( whnf mergesortSerial tenthousand )
     ]

parallelMergesortBenchmark :: Benchmark
parallelMergesortBenchmark
 = bgroup "Parallel"
     [ bench "100"
         ( whnf mergesortParallel hundred )
     , bench "1000"
         ( whnf mergesortParallel thousand )
     , bench "10000"
         ( whnf mergesortParallel tenthousand )
     ]
