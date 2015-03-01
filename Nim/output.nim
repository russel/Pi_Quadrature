# A module providing an output function for all the variants of the "Π by Quadrature" problem.
#
#  Copyright © 2015 Russel Winder

from strutils import format

proc out_s*(name:string, pi:float, n:int, elapseTime:float) =
  echo format(
    "================ $#\n\tπ = $#\n\titeration count = $#\n\telapse time = $#",
    name, pi, n, elapseTime
  )

proc out_p*(name:string, pi:float, n:int, elapseTime:float, taskCount:int) =
  out_s(format("$# ==== task count: $#", name, taskCount), pi, n, elapseTime)
