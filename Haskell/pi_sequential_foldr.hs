--  Haskell implementation of π by Quadrature using a fold (foldr in particular).
--
--  Copyright © 2015  Russel Winder

module Main where

import Output (out)

f :: Double -> Int -> Double -> Double
f delta n t = t + 1.0 / (1.0 + x * x)
  where
    x = ((fromIntegral n) - 0.5) * delta

main :: IO()
main =  out "Sequential Foldr" pi n
  where
    n = 100000000 -- 10 times fewer than C++ for speed reasons.
    delta = 1.0 / (fromIntegral n)
    pi = 4.0 * delta * foldr (f delta) 0.0 [1..n]
