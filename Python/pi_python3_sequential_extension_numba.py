#! /usr/bin/env python3

#  Calculation of π using quadrature. Sequential algorithm using a for loop with a range.
#
#  Copyright © 2008–2012, 2014  Russel Winder

from output import out
from time import time

from numba import autojit

@autojit
def compute(n, delta):
    sum = 0.0
    for i in range(1, n + 1):
        x = (i - 0.5) * delta
        sum += 1.0 / (1.0 + x * x)
    return sum

if __name__ == '__main__':
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * compute(n, delta)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
