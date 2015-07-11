--  Output functions for the Haskell implementation of π by Quadrature.
--
--  Copyright © 2009–2011, 2013–2015  Russel Winder

module Output where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import GHC.Conc (numCapabilities)

out :: String -> Double -> Int -> IO()
out banner pi n = do
  putStrLn ("==================== " ++ show banner)
  startTime <- getCurrentTime
  putStrLn ("\tπ = " ++ show pi)
  putStrLn ("\titeration count = " ++ show n)
  endTime <- getCurrentTime
  putStrLn ("\telapse time = " ++ show (diffUTCTime endTime startTime))

outn :: String -> Double -> Int -> Int -> IO()
outn banner pi n itemCount = do
  out (banner ++ ": item count: " ++ show itemCount) pi n
  putStrLn ("\tprocessor count = " ++ show numCapabilities)
