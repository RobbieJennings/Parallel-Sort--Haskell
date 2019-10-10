module Quicksort where

import Control.Parallel
import Control.Parallel.Strategies

lows :: Ord a => [a] -> [a]
lows [] = []
lows [a] = []
lows (x:xs) = [y | y <- xs, y < x]

highs :: Ord a => [a] -> [a]
highs [] = []
highs [a] = [a]
highs (x:xs) = [y | y <- xs, y >= x]


-- A sequential quicksort
quicksortSerial :: Ord a => [a] -> [a]
quicksortSerial [] = []
quicksortSerial [a] = [a]
quicksortSerial (x:xs) = do
                         let losort = quicksortSerial $ lows (x:xs)
                         let hisort = quicksortSerial $ highs (x:xs)
                         losort ++ x : hisort


-- A parallel quicksort
quicksortParallel :: Ord a => [a] -> [a]
quicksortParallel [] = []
quicksortParallel [a] = [a]
quicksortParallel (x:xs) = runEval $ do
                                     losort <- rpar $ quicksortParallel $ lows (x:xs)
                                     hisort <- rpar $ quicksortParallel $ highs (x:xs)
                                     rseq $ (losort ++ x : hisort)
