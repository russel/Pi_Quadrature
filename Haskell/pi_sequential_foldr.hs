--  Haskell implementation of π by Quadrature
--
--  Copyright © 2015  Russel Winder

module Main where

import Output (out)

f :: Double -> Int -> Double -> Double
f delta n t = t + 1.0 / (1.0 + x * x)
  where
    x = ((fromIntegral n) - 0.5) * delta

main :: IO()
main =  out "Sequential Foldr Where" (4.0 * delta * foldr (f delta) 0.0 [1..n]) n
  where
    n = 100000000 -- 10 times fewer due to speed.
    delta = 1.0 / (fromIntegral n)
