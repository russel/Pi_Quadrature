--  Output functions for the Haskell implementation of π by Quadrature.
--
--  Copyright © 2009–2011, 2013, 2014  Russel Winder

module Output where

import Data.Time.Clock (getCurrentTime, diffUTCTime)
import GHC.Conc (numCapabilities)

out banner pi n = do
  putStrLn ("==================== " ++ show banner)
  startTime <- getCurrentTime
  putStrLn ("\tπ = " ++ show pi)
  --putStrLn ("\tpi = " ++ show pi)
  putStrLn ("\titeration count = " ++ show n)
  endTime <- getCurrentTime
  putStrLn ("\telapse time = " ++ show (diffUTCTime endTime startTime))

outn banner pi n itemCount = do
  out (banner ++ ": item count: " ++ show itemCount) pi n
  putStrLn ("\tprocessor count = " ++ show numCapabilities)
