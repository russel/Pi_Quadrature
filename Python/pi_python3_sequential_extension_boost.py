#! /usr/bin/env python3

#  Calculation of π using quadrature. Sequential algorithm using Boost.
#
#  Copyright © 2008–2012, 2014, 2015  Russel Winder

from output import out
from time import time

from processAll_extension_boost import sequential

if __name__ == '__main__':
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    pi = sequential(n, delta)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
