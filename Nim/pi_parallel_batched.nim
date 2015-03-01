#  Calculation of π using quadrature. Parallel algorithm using a for loop with a range.
#
#  Copyright © 2015 Russel Winder

from output import out_p
from times import epochTime

import threadpool

proc partialSum(id:int, sliceSize:int, delta:float):float =
  result = 0.0
  for i in (1 + id * sliceSize) .. ((id + 1) * sliceSize):
    let x = (toFloat(i) - 0.5) * delta
    result += 1.0 / (1.0 + x * x)

proc sum(sums:openarray[float]):float =
  result = 0.0
  for i in 0 .. sums.high:
    result += sums[i]

proc execute(taskCount:int) =
  const n = 1000000000
  const delta = 1.0 / n
  let startTime = epochTime()
  let sliceSize = toInt(n / taskCount)
  var sums = newSeq[float](taskCount)
  parallel:
    for i in 0 .. sums.high:
      sums[i] = spawn partialSum(i, sliceSize, delta)
  let pi = 4.0 * delta * sum(sums)
  let elapseTime = epochTime() - startTime
  out_p("Parallel Batched", pi, n, elapseTime, taskCount)

when isMainModule:
  execute(1)
  execute(2)
  execute(8)
  execute(32)
