#! /usr/bin/env python3

#  Calculation of π using quadrature. Sequential algorithm using NumPy vectorize.
#
#  Copyright © 2017  Russel Winder

from numpy import arange, double
from output import out
from time import time


def f(i):
    x = (i - 0.5) * delta
    return 1.0 / (1.0 + x * x)


if __name__ == '__main__':
    n = 100000000  # 10 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * f(arange(n, dtype=double)).sum()
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
