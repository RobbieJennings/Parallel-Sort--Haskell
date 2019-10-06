module Tests where

import Test.Framework as TF (Test, defaultMain, testGroup)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit

import Mergesort
import Quicksort

runTests = defaultMain tests

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
    [ testCase "1,2,3"
        ( quicksortSerial [1,2,3] @?= [1,2,3] )
    , testCase "2,3,1"
        ( quicksortSerial [2,3,1] @?= [1,2,3] )
    ]

parallelQuicksortTest :: TF.Test
parallelQuicksortTest
 = testGroup "\nTesting Parllel Quicksort"
    [ testCase "1,2,3"
       ( quicksortParallel [1,2,3] @?= [1,2,3] )
    , testCase "2,3,1"
       ( quicksortParallel [2,3,1] @?= [1,2,3] )
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
    [ testCase "1,2,3"
       ( mergesortSerial [1,2,3] @?= [1,2,3] )
    , testCase "2,3,1"
       ( mergesortSerial [2,3,1] @?= [1,2,3] )
    ]

parallelMergesortTest :: TF.Test
parallelMergesortTest
 = testGroup "\nTesting Parllel Quicksort"
    [ testCase "1,2,3"
       ( mergesortParallel [1,2,3] @?= [1,2,3] )
    , testCase "2,3,1"
       ( mergesortParallel [2,3,1] @?= [1,2,3] )
    ]
