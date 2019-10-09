module Quicksort where

import Control.Parallel
import Control.Parallel.Strategies

-- A sequential quicksort
quicksortSerial :: Ord a => [a] -> [a]
quicksortSerial [] = []
quicksortSerial [a] = [a]
quicksortSerial (x:xs) = do
                         let losort = quicksortSerial [y | y <- xs, y < x]
                         let hisort = quicksortSerial [y | y <- xs, y >= x]
                         losort ++ x : hisort


-- A parallel quicksort
quicksortParallel :: Ord a => [a] -> [a]
quicksortParallel [] = []
quicksortParallel [a] = [a]
quicksortParallel xs = runEval $ qsortParallel xs

qsortParallel :: Ord a => [a] -> Eval [a]
qsortParallel [] = return []
qsortParallel [a] = return [a]
qsortParallel (x:xs) = do
                       losort <- rpar $ runEval $ qsortParallel [y | y <- xs, y < x]
                       hisort <- rpar $ runEval $ qsortParallel [y | y <- xs, y >= x]
                       rseq $ losort ++ x : hisort
