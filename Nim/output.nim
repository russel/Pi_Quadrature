# A module providing an output function for all the variants of the "Π by Quadrature" problem.
#
#  Copyright © 2015 Russel Winder

from strutils import format

proc sequential*(name:string, pi:float, n:int, elapseTime:float) =
  echo format("""================ $#
\tπ = $#
\titeration count = $#
\telapse time = $#""", name, pi, n, elapseTime)
