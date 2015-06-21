#! /usr/bin/env python3

#  Calculation of π using quadrature. Sequential algorithm using reduce.
#
#  Copyright © 2008–2012 Russel Winder

from functools import reduce
from output import out
from time import time


def f(s, i):
    x = (i - 0.5) * delta
    return s + 1.0 / (1.0 + x * x)


if __name__ == '__main__':
    n = 10000000  # 100 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * reduce(f, range(n), 0.0)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
