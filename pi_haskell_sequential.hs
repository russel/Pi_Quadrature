--  Haskell implementation of Pi by Quadrature
--
--  Copyright © 2009--2011 Russel Winder

module Main where

import Data.Time.Clock ( getCurrentTime , diffUTCTime )

piIter :: Int -> Double -> Double -> Double
piIter 0 delta accumulator = 4.0 * accumulator * delta
piIter n delta accumulator = piIter ( n - 1 ) delta ( accumulator + 1.0 / ( 1.0 + x * x )  )
    where
      x = ( ( fromIntegral n ) - 0.5 ) * delta

piQuad :: Int -> Double
piQuad n = piIter n ( 1.0 / ( fromIntegral n ) ) 0.0

main :: IO ( )
main = do
  let n = 1000000000
  startTime <- getCurrentTime
  putStrLn ( "==== Haskell Sequential pi = " ++ show ( piQuad n ) )
  putStrLn ( "==== Haskell Sequential iteration count = " ++ show n )
  endTime <- getCurrentTime
  putStrLn ( "==== Haskell Sequential elapse time = " ++ show ( diffUTCTime endTime startTime ) )
