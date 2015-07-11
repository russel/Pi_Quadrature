--  Haskell implementation of π by Quadrature
--
--  Copyright © 2015  Russel Winder

module Main where

import Output (out)

f delta n = 1.0 / (1.0 + x * x)
  where
    x = ((fromIntegral n) - 0.5) * delta

main :: IO()
main =  out "Sequential Map Where" (4.0 * delta * sum (map (f delta) [1..n])) n
  where
    n = 1000000000
    delta = 1.0 / (fromIntegral n)
