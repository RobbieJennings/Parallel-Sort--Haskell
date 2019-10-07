module Quicksort where

import Control.Parallel

-- A sequential quicksort
quicksortSerial :: Ord a => [a] -> [a]
quicksortSerial [] = []
quicksortSerial (x:xs) = losort ++ x : hisort
  where
  losort = quicksortSerial [y | y <- xs, y < x]
  hisort = quicksortSerial [y | y <- xs, y >= x]


-- A parallel quicksort
quicksortParallel :: Ord a => [a] -> [a]
quicksortParallel [] = []
quicksortParallel (x:xs) = par losort (pseq hisort (losort ++ x : hisort))
  where
  losort = quicksortParallel [y | y <- xs, y < x]
  hisort = quicksortParallel [y | y <- xs, y >= x]
