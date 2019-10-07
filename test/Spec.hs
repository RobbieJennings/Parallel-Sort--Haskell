module Main where

import Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit
import System.Random
import Data.Sort

import Mergesort
import Quicksort

hundred = (take 100 (randomRs (0,100) (mkStdGen 42)))::[Int]
thousand = (take 1000 (randomRs (0,100) (mkStdGen 42)))::[Int]
tenthousand = (take 10000 (randomRs (0,100) (mkStdGen 42)))::[Int]

hundredSorted = sort hundred
thousandSorted = sort thousand
tenthousandSorted = sort tenthousand

main = defaultMain tests

tests :: [TF.Test]
tests = [ testGroup "\nTesting Sorting Algorithms"
          [ quicksortTest
          , mergesortTest
          ]
        ]


-------TESTING QUICKSORT--------

quicksortTest :: TF.Test
quicksortTest
 = testGroup "\nTesting Quicksort"
    [ serialQuicksortTest
    , parallelQuicksortTest
    ]

serialQuicksortTest :: TF.Test
serialQuicksortTest
 = testGroup "\nTesting Serial Quicksort"
    [ testCase "100"
        ( quicksortSerial hundred @?= hundredSorted )
    , testCase "1000"
        ( quicksortSerial thousand @?= thousandSorted )
    , testCase "10000"
        ( quicksortSerial tenthousand @?= tenthousandSorted )
    ]

parallelQuicksortTest :: TF.Test
parallelQuicksortTest
 = testGroup "\nTesting Parllel Quicksort"
     [ testCase "100"
         ( quicksortParallel hundred @?= hundredSorted )
     , testCase "1000"
         ( quicksortParallel thousand @?= thousandSorted )
     , testCase "10000"
         ( quicksortParallel tenthousand @?= tenthousandSorted )
     ]


-------TESTING MERGESORT--------

mergesortTest :: TF.Test
mergesortTest
 = testGroup "\nTesting Mergesort"
    [ serialMergesortTest
    , parallelMergesortTest
    ]

serialMergesortTest :: TF.Test
serialMergesortTest
 = testGroup "\nTesting Serial Mergesort"
     [ testCase "100"
         ( mergesortSerial hundred @?= hundredSorted )
     , testCase "1000"
         ( mergesortSerial thousand @?= thousandSorted )
     , testCase "10000"
         ( mergesortSerial tenthousand @?= tenthousandSorted )
     ]

parallelMergesortTest :: TF.Test
parallelMergesortTest
 = testGroup "\nTesting Parllel Quicksort"
     [ testCase "100"
         ( mergesortParallel hundred @?= hundredSorted )
     , testCase "1000"
         ( mergesortParallel thousand @?= thousandSorted )
     , testCase "10000"
         ( mergesortParallel tenthousand @?= tenthousandSorted )
     ]
