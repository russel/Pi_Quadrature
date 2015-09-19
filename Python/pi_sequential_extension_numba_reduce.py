#! /usr/bin/env python3

#  Calculation of π using quadrature. Sequential algorithm using a for loop with a range.
#
#  Copyright © 2008–2012, 2014, 2015  Russel Winder

from functools import reduce
from output import out
from time import time

from numba import jit


@jit(nopython=True)
def compute(n, delta):
    def f(t, i):
        x = (i - 0.5) * delta
        return t + 1.0 / (1.0 + x * x)
    return reduce(f, range(1, n + 1))


if __name__ == '__main__':
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * compute(n, delta)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
