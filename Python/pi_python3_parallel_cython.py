#! /usr/bin/env python3

#  Calculation of π using quadrature. Uses Cython code to handle the loop and it's parallelization..
#
#  Copyright © 2014 Russel Winder

from output import out
from time import time

from processAll_cython import parallel

if __name__ == '__main__':
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * parallel(n, delta)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
