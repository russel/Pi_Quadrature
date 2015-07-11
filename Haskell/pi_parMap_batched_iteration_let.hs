--  Haskell implementation of π by Quadrature.  Parallel version using parMap.
--
--  Copyright © 2009–2011, 2013–2015  Russel Winder

module Main where

import Control.Parallel.Strategies (rseq, parMap)

import Output (outn)

piIter :: Int -> Int -> Double -> Double -> Double
piIter  n to delta accumulator
  | n > to = 4.0 * delta * accumulator
  | otherwise =
      let
        nPlus1 = n + 1
        x = ((fromIntegral n) - 0.5) * delta
        value = accumulator + 1.0 / (1.0 + x * x)
      in
        piIter nPlus1 to delta value

piQuadSlice :: Double -> Int -> Int -> Double
piQuadSlice delta sliceSize index =
  let
    start = 1 +  index * sliceSize
    end =  (index + 1) * sliceSize
  in
    piIter start end delta 0.0

execute :: Int -> IO ()
execute numberOfSlices =
  let
    n = 1000000000
    delta = 1.0 / (fromIntegral n)
    sliceSize = n `div` numberOfSlices
    pi = sum (parMap rseq (piQuadSlice delta sliceSize) [0 .. (numberOfSlices - 1)])
  in
    outn "ParMap Batched Iteration Let" pi n numberOfSlices

main :: IO ()
main = do
  execute 1
  execute 2
  execute 8
  execute 32
