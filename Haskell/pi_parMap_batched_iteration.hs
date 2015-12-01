--  Haskell implementation of π by Quadrature.  Parallel version using parMap.
--
--  Copyright © 2009–2011, 2013–2015  Russel Winder

module Main where

import Control.Parallel.Strategies (rseq, parMap)

import Output (outn)

piIter :: Int -> Int -> Double -> Double -> Double
piIter  n to delta accumulator
  | n > to = 4.0 * delta * accumulator
  | otherwise = piIter (n + 1) to delta value
  where
    x = ((fromIntegral n) - 0.5) * delta
    value = accumulator + 1.0 / (1.0 + x * x)

piQuadSlice :: Double -> Int -> Int -> Double
piQuadSlice delta sliceSize index = piIter start end delta 0.0
  where
    start = 1 +  index * sliceSize
    end =  (index + 1) * sliceSize

execute :: Int -> IO ()
execute numberOfSlices =  outn "ParMap Batched Tail Recursion" pi n numberOfSlices
  where
    n = 100000000 -- 10 times fewer than C++ for speed reasons.
    delta = 1.0 / (fromIntegral n)
    sliceSize = n `div` numberOfSlices
    pi = sum (parMap rseq (piQuadSlice delta sliceSize) [0 .. (numberOfSlices - 1)])

main :: IO ()
main = do
  execute 1
  execute 2
  execute 8
  execute 32
