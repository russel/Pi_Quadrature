--  Haskell implementation of Pi by Quadrature
--
--  Copyright © 2009--2011 Russel Winder

module Main where

import Data.Time.Clock ( getCurrentTime , diffUTCTime )

piIter :: Int -> Double -> Double -> Double
piIter 0 delta accumulator = 4.0 * accumulator * delta
piIter n delta accumulator =
    let
        nMinus1 = n -1
        x = ( ( fromIntegral n ) - 0.5 ) * delta
        value = accumulator + 1.0 / ( 1.0 + x * x )
    in
      nMinus1 `seq` x `seq` value `seq` piIter nMinus1 delta value
 
piQuad :: Int -> Double
piQuad n = piIter n ( 1.0 / ( fromIntegral n ) ) 0.0

main :: IO ( )
main = do
  let n = 1000000000
  startTime <- getCurrentTime
  putStrLn ( "==== Haskell Sequential Forced Strict pi = " ++ show ( piQuad n ) )
  endTime <- getCurrentTime
  putStrLn ( "==== Haskell Sequential Forced Strict iteration count = " ++ show n )
  putStrLn ( "==== Haskell Sequential Forced Strict elapse time = " ++ show ( diffUTCTime endTime startTime ) )
