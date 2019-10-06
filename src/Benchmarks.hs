module Benchmarks where

import Criterion.Main

import Mergesort
import Quicksort

runBenchmarks = defaultMain benchmarks

benchmarks = [ bgroup "Benchmarking"
               [ quicksortBenchmark
               , mergesortBenchmark
               ]
             ]


-------BENCHMARKING QUICKSORT--------

quicksortBenchmark
 = bgroup "Quicksort"
    [ serialQuicksortBenchmark
    , parallelQuicksortBenchmark
    ]

serialQuicksortBenchmark
 = bgroup "Serial"
    [ bench "1,2,3"
        ( whnf quicksortSerial [1,2,3] )
    , bench "2,3,1"
        ( whnf quicksortSerial [2,3,1] )
    ]

parallelQuicksortBenchmark
 = bgroup "Parallel"
    [ bench "1,2,3"
        ( whnf quicksortParallel [1,2,3] )
    , bench "2,3,1"
        ( whnf quicksortParallel [2,3,1] )
    ]


-------BENCHMARKING MERGESORT--------

mergesortBenchmark
 = bgroup "Mergesort"
    [ serialMergesortBenchmark
    , parallelMergesortBenchmark
    ]

serialMergesortBenchmark
 = bgroup "Serial"
    [ bench "1,2,3"
        ( whnf mergesortSerial [1,2,3] )
    , bench "2,3,1"
        ( whnf mergesortSerial [2,3,1] )
    ]

parallelMergesortBenchmark
 = bgroup "Parallel"
    [ bench "1,2,3"
        ( whnf mergesortParallel [1,2,3] )
    , bench "2,3,1"
        ( whnf mergesortParallel [2,3,1] )
    ]
