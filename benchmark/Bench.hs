module Main where

import Criterion.Main

import Mergesort
import Quicksort

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
    [ bench "1,2,3"
        ( whnf quicksortSerial [1,2,3] )
    , bench "2,3,1"
        ( whnf quicksortSerial [2,3,1] )
    ]

parallelQuicksortBenchmark :: Benchmark
parallelQuicksortBenchmark
 = bgroup "Parallel"
    [ bench "1,2,3"
        ( whnf quicksortParallel [1,2,3] )
    , bench "2,3,1"
        ( whnf quicksortParallel [2,3,1] )
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
    [ bench "1,2,3"
        ( whnf mergesortSerial [1,2,3] )
    , bench "2,3,1"
        ( whnf mergesortSerial [2,3,1] )
    ]

parallelMergesortBenchmark :: Benchmark
parallelMergesortBenchmark
 = bgroup "Parallel"
    [ bench "1,2,3"
        ( whnf mergesortParallel [1,2,3] )
    , bench "2,3,1"
        ( whnf mergesortParallel [2,3,1] )
    ]
