--  Haskell implementation of π by Quadrature
--
--  Copyright © 2015  Russel Winder

module Main where

import Output (out)

f :: Double -> Int -> Double -> Double
f delta n t =
  let x = ((fromIntegral n) - 0.5) * delta
  in t + 1.0 / (1.0 + x * x)

main :: IO()
main =
  let
    n = 100000000 -- 10 times fewer for speed reasons.
    delta = 1.0 / (fromIntegral n)
  in
    out "Sequential Foldr Let" (4.0 * delta * foldr (f delta) 0.0 [1..n]) n
