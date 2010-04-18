--  Haskell implementation of Pi by Quadrature.  This uses threads to parallelize.
--
--  Copyright © 2009 Russel Winder

module Main where

import Data.Time.Clock ( getCurrentTime , diffUTCTime )
import GHC.Conc ( numCapabilities )
import Control.Concurrent ( forkIO , newEmptyMVar , putMVar, takeMVar )

piIter :: Integer -> Integer -> Double -> Double -> Double
piIter  n to delta accumulator
    | n > to = 4.0 * accumulator * delta
    | otherwise = 
        let
          nPlus1 = n + 1
          x = ( ( fromIntegral n ) - 0.5 ) * delta
          value = accumulator + 1.0 / ( 1.0 + x * x )        
        in
          piIter nPlus1 to delta value

spawnWorkersAndSum :: Integer -> Double -> Integer -> IO ( Double )
spawnWorkersAndSum 0 _ _ = return ( 0.0 )
spawnWorkersAndSum i delta sliceSize = do
  forkedWorkerValue <- newEmptyMVar
  forkIO $ do
    -- Ensure strictness so as to ensure the computation actually happens in the spawned thread and not the
    -- parent thread.
    let value = piIter ( 1 +  ( i - 1 ) * sliceSize ) ( i * sliceSize )  delta 0.0
    putMVar forkedWorkerValue ( value `seq` value ) 
  y <- spawnWorkersAndSum ( i - 1 ) delta sliceSize
  x <- takeMVar forkedWorkerValue
  return ( x + y )

execute :: Integer -> IO ( )
execute numberOfSlices = do
  let n = 1000000000
  let delta = 1.0 / ( fromIntegral n )
  startTime <- getCurrentTime
  let sliceSize = n `div` numberOfSlices
  pi <- spawnWorkersAndSum numberOfSlices delta sliceSize
  --  Don't get the time here since nothing has actually been computed as yet since pi has not been used.
  print ( "==== Haskell Threads pi = " ++ show pi )
  print ( "==== Haskell Threads iteration count = " ++ show n )
  endTime <- getCurrentTime
  print ( "==== Haskell Threads elapse time = " ++ show ( diffUTCTime endTime startTime ) )
  print ( "==== Haskell Threads slice count = " ++ show numberOfSlices )
  print ( "==== Haskell Threads processor count = " ++ show numCapabilities )

main :: IO ( )
main = do
  execute 1
  print ""
  execute 2
  print ""
  execute 8
  print ""
  execute 32
