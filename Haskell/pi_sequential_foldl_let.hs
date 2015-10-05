--  Haskell implementation of π by Quadrature
--
--  Copyright © 2015  Russel Winder

module Main where

import Data.List (foldl')

import Output (out)

f :: Double -> Double -> Int -> Double
f delta t n =
  let x = ((fromIntegral n) - 0.5) * delta
  in t + 1.0 / (1.0 + x * x)

main :: IO()
main =
  let
    n = 100000000 -- 10 times fewer for speed reasons.
    delta = 1.0 / (fromIntegral n)
  in
    out "Sequential Foldl Let" (4.0 * delta * foldl' (f delta) 0.0 [1..n]) n
