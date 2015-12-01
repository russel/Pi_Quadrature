--  Haskell implementation of π by Quadrature using a fold (foldl' in particular).
--
--  Copyright © 2015  Russel Winder

module Main where

import Data.List (foldl')

import Output (out)

f :: Double -> Double -> Int -> Double
f delta t n = t + 1.0 / (1.0 + x * x)
  where
    x = ((fromIntegral n) - 0.5) * delta

main :: IO()
main =  out "Sequential Foldl'" pi n
  where
    n = 100000000 -- 10 times fewer than C++ for speed reasons.
    delta = 1.0 / (fromIntegral n)
    pi = 4.0 * delta * foldl' (f delta) 0.0 [1..n]
