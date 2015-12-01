--  Haskell implementation of π by Quadrature using a map and then a reduce (actually sum).
--
--  Copyright © 2015  Russel Winder

module Main where

import Output (out)

f :: Double -> Int -> Double
f delta n = 1.0 / (1.0 + x * x)
  where
    x = ((fromIntegral n) - 0.5) * delta

main :: IO()
main =  out "Sequential Map Sum" pi n
  where
    n = 100000000 -- 10 times fewer than C++ for speed reasons.
    delta = 1.0 / (fromIntegral n)
    pi = 4.0 * delta * sum (map (f delta) [1..n])
