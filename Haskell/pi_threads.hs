--  Haskell implementation of π by Quadrature.  This uses threads to parallelize.
--
--  Copyright © 2009–2011, 2013, 2015  Russel Winder

module Main where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)

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

spawnWorkersAndSum :: Int -> Double -> Int -> IO(Double)
spawnWorkersAndSum 0 _ _ = return (0.0)
spawnWorkersAndSum i delta sliceSize = do
  forkedWorkerValue <- newEmptyMVar
  forkIO $ do
    -- Ensure strictness so as to ensure the computation actually happens in the spawned thread and not the
    -- parent thread.
    let value = piIter (1 +  (i - 1) * sliceSize) (i * sliceSize)  delta 0.0
    putMVar forkedWorkerValue (value `seq` value)
  y <- spawnWorkersAndSum (i - 1) delta sliceSize
  x <- takeMVar forkedWorkerValue
  return (x + y)

execute :: Int -> IO()
execute numberOfSlices = do
  let n = 100000000 -- 10 times fewer due to speed.
  let delta = 1.0 / (fromIntegral n)
  let sliceSize = n `div` numberOfSlices
  pi <- spawnWorkersAndSum numberOfSlices delta sliceSize
  outn "Threads" pi n numberOfSlices

main :: IO()
main = do
  execute 1
  execute 2
  execute 8
  execute 32
