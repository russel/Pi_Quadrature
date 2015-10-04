--  Haskell implementation of π by Quadrature
--
--  Copyright © 2015  Russel Winder

module Main where

import Output (out)

f :: Double -> Double -> Int -> Double
f delta t n = t + 1.0 / (1.0 + x * x)
  where
    x = ((fromIntegral n) - 0.5) * delta

main :: IO()
main =  out "Sequential Foldl Where" (4.0 * delta * foldl (f delta) 0.0 [1..n]) n
  where
    n = 100000000 -- 10 times fewer due to speed.
    delta = 1.0 / (fromIntegral n)
