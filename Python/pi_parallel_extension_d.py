#! /usr/bin/env python3

#  Calculation of π using quadrature. Uses D code to handle the computation and it's parallelization.
#
#  Copyright © 2014, 2015  Russel Winder

from output import out
from time import time

from processAll_extension_d import parallel

if __name__ == '__main__':
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    pi = parallel(n, delta)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
