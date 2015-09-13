--  Haskell implementation of π by Quadrature
--
--  Copyright © 2015  Russel Winder

module Main where

import Output (out)

f :: Double -> Int -> Double
f delta n = 1.0 / (1.0 + x * x)
  where
    x = ((fromIntegral n) - 0.5) * delta

main :: IO()
main =  out "Sequential Map Where" (4.0 * delta * sum (map (f delta) [1..n])) n
  where
    n = 100000000 -- 10 times fewer due to speed.
    delta = 1.0 / (fromIntegral n)
