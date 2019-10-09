module Mergesort where

import Control.Parallel
import Control.Parallel.Strategies

firstHalf xs = take (div (length xs) 2) xs
secondHalf xs = drop (div (length xs) 2) xs

-- A sequential mergesort
mergesortSerial :: Ord a => [a] -> [a]
mergesortSerial [] = []
mergesortSerial [a] = [a]
mergesortSerial xs = do
                     let first = mergesortSerial $ firstHalf xs
                     let second = mergesortSerial $ secondHalf xs
                     mergeSerial first second

mergeSerial :: Ord a => [a] -> [a] -> [a]
mergeSerial xs [] = xs
mergeSerial [] ys = ys
mergeSerial (x:xs) (y:ys) | x <= y    = x:mergeSerial xs (y:ys)
                          | otherwise = y:mergeSerial (x:xs) ys


-- A parallel mergesort
mergesortParallel :: Ord a => [a] -> [a]
mergesortParallel []  = []
mergesortParallel [a] = [a]
mergesortParallel xs  = runEval $ msortParallel xs

msortParallel :: Ord a => [a] -> Eval [a]
msortParallel [] = return []
msortParallel [a] = return [a]
msortParallel xs = do
                   first <- rpar $ runEval $ msortParallel $ firstHalf xs
                   second <- rseq $ runEval $ msortParallel $ secondHalf xs
                   rseq $ mergeParallel first second

mergeParallel :: Ord a => [a] -> [a] -> [a]
mergeParallel xs [] = xs
mergeParallel [] ys = ys
mergeParallel (x:xs) (y:ys) | x <= y    = x:mergeParallel xs (y:ys)
                            | otherwise = y:mergeParallel (x:xs) ys
