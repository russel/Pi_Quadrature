#  Calculation of π using quadrature. Sequential algorithm using a for loop with a range.
#
#  Copyright © 2015 Russel Winder

from output import out_s
from times import epochTime

when isMainModule:
  const n = 100000000 # 10 times fewer due to speed problems
  const delta = 1.0 / n
  let startTime = epochTime()
  var sum:float = 0.0
  for i in 1 .. n:
    let x = (toFloat(i) - 0.5) * delta
    sum += 1.0 / (1.0 + x * x)
  let pi = 4.0 * delta * sum
  let elapseTime = epochTime() - startTime
  out_s("Sequential", pi, n, elapseTime)
