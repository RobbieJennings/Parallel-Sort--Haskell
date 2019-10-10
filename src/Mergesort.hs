module Mergesort where

import Control.Parallel
import Control.Parallel.Strategies

firstHalf :: Ord a => [a] -> [a]
firstHalf [] = []
firstHalf [a] = []
firstHalf xs = take (div (length xs) 2) xs

secondHalf :: Ord a => [a] -> [a]
secondHalf [] = []
secondHalf [a] = [a]
secondHalf xs = drop (div (length xs) 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys


-- A sequential mergesort
mergesortSerial :: Ord a => [a] -> [a]
mergesortSerial [] = []
mergesortSerial [a] = [a]
mergesortSerial xs = do
                     let first = mergesortSerial $ firstHalf xs
                     let second = mergesortSerial $ secondHalf xs
                     merge first second


-- A parallel mergesort
mergesortParallel :: Ord a => [a] -> [a]
mergesortParallel [] = []
mergesortParallel [a] = [a]
mergesortParallel xs = runEval $ do
                                 first <- rpar $ mergesortParallel $ firstHalf xs
                                 second <- rpar $ mergesortParallel $ secondHalf xs
                                 rseq $ merge first second
