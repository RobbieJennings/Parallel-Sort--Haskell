module Mergesort where

firstHalf  xs = take (div (length xs) 2) xs
secondHalf xs = drop (div (length xs) 2) xs

-- A sequential mergesort
mergesortSerial :: Ord a => [a] -> [a]
mergesortSerial [] = []
mergesortSerial [a] = [a]
mergesortSerial xs = mergeSerial
                      (mergesortSerial (firstHalf xs))
                      (mergesortSerial (secondHalf xs))

mergeSerial :: Ord a => [a] -> [a] -> [a]
mergeSerial xs [] = xs
mergeSerial [] ys = ys
mergeSerial (x:xs) (y:ys) | x <= y    = x:mergeSerial xs (y:ys)
                          | otherwise = y:mergeSerial (x:xs) ys


-- A parallel mergesort
mergesortParallel :: Ord a => [a] -> [a]
mergesortParallel [] = []
mergesortParallel [a] = [a]
mergesortParallel xs = mergeParallel
                        (mergesortParallel (firstHalf xs))
                        (mergesortParallel (secondHalf xs))


mergeParallel :: Ord a => [a] -> [a] -> [a]
mergeParallel xs [] = xs
mergeParallel [] ys = ys
mergeParallel (x:xs) (y:ys) | x <= y    = x:mergeParallel xs (y:ys)
                            | otherwise = y:mergeParallel (x:xs) ys
