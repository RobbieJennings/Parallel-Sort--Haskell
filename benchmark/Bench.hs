module Main where

import Criterion.Main
import System.Random

import Mergesort
import Quicksort

main
  = do
    ten <- sequence $ replicate 10 $ randomRIO (1,100::Int)
    hundred <- sequence $ replicate 100 $ randomRIO (1,100::Int)
    thousand <- sequence $ replicate 1000 $ randomRIO (1,100::Int)
    tenthousand <- sequence $ replicate 10000 $ randomRIO (1,100::Int)
    hundredthousand <- sequence $ replicate 100000 $ randomRIO (1,100::Int)
    defaultMain $ benchmarks ten hundred thousand tenthousand hundredthousand

benchmarks :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [Benchmark]
benchmarks ten hundred thousand tenthousand hundredthousand
  = [ bgroup "Quicksort"
      [ serialQuicksort ten hundred thousand tenthousand hundredthousand
      , parallelQuicksort ten hundred thousand tenthousand hundredthousand
      ]
      , bgroup "Mergesort"
      [ serialMergesort ten hundred thousand tenthousand hundredthousand
      , parallelMergesort ten hundred thousand tenthousand hundredthousand
      ]
    ]


-------BENCHMARKING QUICKSORT--------

serialQuicksort :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Benchmark
serialQuicksort ten hundred thousand tenthousand hundredthousand
  = bgroup "Serial"
    [ bench "10"
      ( whnf quicksortSerial ten )
    , bench "100"
      ( whnf quicksortSerial hundred )
    , bench "1000"
      ( whnf quicksortSerial thousand )
    , bench "10000"
      ( whnf quicksortSerial tenthousand )
    , bench "100000"
      ( whnf quicksortSerial hundredthousand )
    ]

parallelQuicksort :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Benchmark
parallelQuicksort ten hundred thousand tenthousand hundredthousand
  = bgroup "Parallel"
    [ bench "10"
      ( whnf quicksortParallel ten )
    , bench "100"
      ( whnf quicksortParallel hundred )
    , bench "1000"
      ( whnf quicksortParallel thousand )
    , bench "10000"
      ( whnf quicksortParallel tenthousand )
    , bench "100000"
      ( whnf quicksortParallel hundredthousand )
    ]


-------BENCHMARKING MERGESORT--------

serialMergesort :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Benchmark
serialMergesort ten hundred thousand tenthousand hundredthousand
  = bgroup "Serial"
    [ bench "10"
      ( whnf mergesortSerial ten )
    , bench "100"
      ( whnf mergesortSerial hundred )
    , bench "1000"
      ( whnf mergesortSerial thousand )
    , bench "10000"
      ( whnf mergesortSerial tenthousand )
    , bench "100000"
      ( whnf mergesortSerial hundredthousand )
    ]

parallelMergesort :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> Benchmark
parallelMergesort ten hundred thousand tenthousand hundredthousand
  = bgroup "Parallel"
    [ bench "10"
      ( whnf mergesortParallel ten )
    , bench "100"
      ( whnf mergesortParallel hundred )
    , bench "1000"
      ( whnf mergesortParallel thousand )
    , bench "10000"
      ( whnf mergesortParallel tenthousand )
    , bench "100000"
      ( whnf mergesortParallel hundredthousand )
    ]
