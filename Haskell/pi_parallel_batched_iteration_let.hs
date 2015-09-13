--  Haskell implementation of π by Quadrature.  This implementation tries to parallelize.
--
--  Copyright © 2009–2011, 2013, 2015  Russel Winder

module Main where

import Control.Parallel (par)

import Output (outn)

piIter :: Int -> Int -> Double -> Double -> Double
piIter  n to delta accumulator
  | n > to = 4.0 * delta * accumulator
  | otherwise =
      let
        x = ((fromIntegral n) - 0.5) * delta
        value = accumulator + 1.0 / (1.0 + x * x)
      in
        piIter (n + 1) to delta value

piQuadSlice :: Double -> Int -> Int -> Double
piQuadSlice delta sliceSize index =
  let
    start = 1 +  index * sliceSize
    end = (index + 1) * sliceSize
  in
    piIter start end delta 0.0

parallelMap :: (a -> b) -> [a] -> [b]
parallelMap f (x : xs) =
    let r = f x
    in r `par` r : parallelMap f xs
parallelMap _ _  = []

execute :: Int -> IO()
execute numberOfSlices =
  let
    n = 100000000 -- 10 times fewer for speed reasons.
    delta = 1.0 / (fromIntegral n)
    sliceSize = n `div` numberOfSlices
    pi = sum (parallelMap (piQuadSlice delta sliceSize) [0 .. (numberOfSlices - 1)])
  in
    outn "Parallel Batched Iteration Let" pi n numberOfSlices

main :: IO()
main = do
  execute 1
  execute 2
  execute 8
  execute 32
