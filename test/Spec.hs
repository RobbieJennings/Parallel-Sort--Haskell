module Main where

import Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import System.Random
import Data.Sort

import Mergesort
import Quicksort

main
  = do
    ten <- sequence $ replicate 10 $ randomRIO (1,100::Int)
    hundred <- sequence $ replicate 100 $ randomRIO (1,100::Int)
    thousand <- sequence $ replicate 1000 $ randomRIO (1,100::Int)
    tenthousand <- sequence $ replicate 10000 $ randomRIO (1,100::Int)
    hundredthousand <- sequence $ replicate 100000 $ randomRIO (1,100::Int)
    defaultMain $ tests ten hundred thousand tenthousand hundredthousand

tests :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> [TF.Test]
tests ten hundred thousand tenthousand hundredthousand
  = [ testGroup "\nQuicksort"
      [ serialQuicksort ten hundred thousand tenthousand hundredthousand
      , parallelQuicksort ten hundred thousand tenthousand hundredthousand
      ]
      , testGroup "\nMergesort"
      [ serialMergesort ten hundred thousand tenthousand hundredthousand
      , parallelMergesort ten hundred thousand tenthousand hundredthousand
      ]
    ]


-------BENCHMARKING QUICKSORT--------

serialQuicksort :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> TF.Test
serialQuicksort ten hundred thousand tenthousand hundredthousand
  = testGroup "Serial"
    [ testCase "10"
      ( quicksortSerial ten @?= sort ten )
      , testCase "100"
      ( quicksortSerial hundred @?= sort hundred )
      , testCase "1000"
      ( quicksortSerial thousand @?= sort thousand )
      , testCase "10000"
      ( quicksortSerial tenthousand @?= sort tenthousand )
      , testCase "100000"
      ( quicksortSerial hundredthousand @?= sort hundredthousand )
    ]

parallelQuicksort :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> TF.Test
parallelQuicksort ten hundred thousand tenthousand hundredthousand
  = testGroup "Parallel"
    [ testCase "10"
      ( quicksortParallel ten @?= sort ten )
      , testCase "100"
      ( quicksortParallel hundred @?= sort hundred )
      , testCase "1000"
      ( quicksortParallel thousand @?= sort thousand )
      , testCase "10000"
      ( quicksortParallel tenthousand @?= sort tenthousand )
      , testCase "100000"
      ( quicksortParallel hundredthousand @?= sort hundredthousand )
    ]


-------BENCHMARKING MERGESORT--------

serialMergesort :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> TF.Test
serialMergesort ten hundred thousand tenthousand hundredthousand
  = testGroup "Serial"
    [ testCase "10"
      ( mergesortSerial ten @?= sort ten )
      , testCase "100"
      ( mergesortSerial hundred @?= sort hundred )
      , testCase "1000"
      ( mergesortSerial thousand @?= sort thousand )
      , testCase "10000"
      ( mergesortSerial tenthousand @?= sort tenthousand )
      , testCase "100000"
      ( mergesortSerial hundredthousand @?= sort hundredthousand )
    ]

parallelMergesort :: [Int] -> [Int] -> [Int] -> [Int] -> [Int] -> TF.Test
parallelMergesort ten hundred thousand tenthousand hundredthousand
  = testGroup "Parallel"
    [ testCase "10"
      ( mergesortParallel ten @?= sort ten )
      , testCase "100"
      ( mergesortParallel hundred @?= sort hundred )
      , testCase "1000"
      ( mergesortParallel thousand @?= sort thousand )
      , testCase "10000"
      ( mergesortParallel tenthousand @?= sort tenthousand )
      , testCase "100000"
      ( mergesortParallel hundredthousand @?= sort hundredthousand )
    ]
