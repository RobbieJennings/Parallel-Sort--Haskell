module Main where

import Criterion.Main
import System.Random

import Mergesort
import Quicksort

main = do
  g <- getStdGen
  let hundred = (take 100 (randomRs (0,100) g))::[Int]
  let thousand = (take 1000 (randomRs (0,100) g))::[Int]
  let tenthousand = (take 10000 (randomRs (0,100) g))::[Int]
  defaultMain $ benchmarks hundred thousand tenthousand

benchmarks :: [Int] -> [Int] -> [Int] -> [Benchmark]
benchmarks hundred thousand tenthousand =
  [ bgroup "Quicksort"
    [ serialQuicksort hundred thousand tenthousand
    , parallelQuicksort hundred thousand tenthousand
    ]
  , bgroup "Mergesort"
    [ serialQuicksort hundred thousand tenthousand
    , parallelQuicksort hundred thousand tenthousand
    ]
  ]


-------BENCHMARKING QUICKSORT--------

serialQuicksort :: [Int] -> [Int] -> [Int] -> Benchmark
serialQuicksort hundred thousand tenthousand
 = bgroup "Serial"
    [ bench "100"
        ( whnf quicksortSerial hundred )
    , bench "1000"
        ( whnf quicksortSerial thousand )
    , bench "10000"
        ( whnf quicksortSerial tenthousand )
    ]

parallelQuicksort :: [Int] -> [Int] -> [Int] -> Benchmark
parallelQuicksort hundred thousand tenthousand
 = bgroup "Parallel"
     [ bench "100"
         ( whnf quicksortParallel hundred )
     , bench "1000"
         ( whnf quicksortParallel thousand )
     , bench "10000"
         ( whnf quicksortParallel tenthousand )
     ]


-------BENCHMARKING MERGESORT--------

serialMergesort :: [Int] -> [Int] -> [Int] -> Benchmark
serialMergesort hundred thousand tenthousand
 = bgroup "Serial"
     [ bench "100"
         ( whnf mergesortSerial hundred )
     , bench "1000"
         ( whnf mergesortSerial thousand )
     , bench "10000"
         ( whnf mergesortSerial tenthousand )
     ]

parallelMergesort :: [Int] -> [Int] -> [Int] -> Benchmark
parallelMergesort hundred thousand tenthousand
 = bgroup "Parallel"
     [ bench "100"
         ( whnf mergesortParallel hundred )
     , bench "1000"
         ( whnf mergesortParallel thousand )
     , bench "10000"
         ( whnf mergesortParallel tenthousand )
     ]
