module Main where

import Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import System.Random
import Data.Sort

import Mergesort
import Quicksort

main = do
  g <- getStdGen
  let hundred = (take 100 (randomRs (0,100) g))::[Int]
  let thousand = (take 1000 (randomRs (0,100) g))::[Int]
  let tenthousand = (take 10000 (randomRs (0,100) g))::[Int]
  defaultMain $ tests hundred thousand tenthousand

tests :: [Int] -> [Int] -> [Int] -> [TF.Test]
tests hundred thousand tenthousand =
 [ testGroup "\nQuicksort"
   [ serialQuicksort hundred thousand tenthousand
   , parallelQuicksort hundred thousand tenthousand
   ]
 , testGroup "\nMergesort"
   [ serialQuicksort hundred thousand tenthousand
   , parallelQuicksort hundred thousand tenthousand
   ]
 ]


-------BENCHMARKING QUICKSORT--------

serialQuicksort :: [Int] -> [Int] -> [Int] -> TF.Test
serialQuicksort hundred thousand tenthousand
  = testGroup "Serial"
     [ testCase "100"
         ( quicksortSerial hundred @?= sort hundred )
     , testCase "1000"
         ( quicksortSerial thousand @?= sort thousand )
     , testCase "10000"
         ( quicksortSerial tenthousand @?= sort tenthousand )
     ]

parallelQuicksort :: [Int] -> [Int] -> [Int] -> TF.Test
parallelQuicksort hundred thousand tenthousand
  = testGroup "Parallel"
    [ testCase "100"
        ( quicksortParallel hundred @?= sort hundred )
    , testCase "1000"
        ( quicksortParallel thousand @?= sort thousand )
    , testCase "10000"
        ( quicksortParallel tenthousand @?= sort tenthousand )
    ]


-------BENCHMARKING MERGESORT--------

serialMergesort :: [Int] -> [Int] -> [Int] -> TF.Test
serialMergesort hundred thousand tenthousand
  = testGroup "Serial"
     [ testCase "100"
         ( mergesortSerial hundred @?= sort hundred )
     , testCase "1000"
         ( mergesortSerial thousand @?= sort thousand )
     , testCase "10000"
         ( mergesortSerial tenthousand @?= sort tenthousand )
     ]

parallelMergesort :: [Int] -> [Int] -> [Int] -> TF.Test
parallelMergesort hundred thousand tenthousand
  = testGroup "Parallel"
    [ testCase "100"
        ( mergesortParallel hundred @?= sort hundred )
    , testCase "1000"
        ( mergesortParallel thousand @?= sort thousand )
    , testCase "10000"
        ( mergesortParallel tenthousand @?= sort tenthousand )
    ]
