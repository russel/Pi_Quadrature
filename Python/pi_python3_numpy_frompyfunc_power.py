#! /usr/bin/env python3

#  Calculation of π using quadrature. Sequential algorithm using NumPy frompyfunc.
#
#  Copyright © 2008–2013  Russel Winder

from numpy import arange, double, frompyfunc
from output import out
from time import time

if __name__ == '__main__':
    n = 10000000  # 100 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * frompyfunc(lambda i: 1.0 / (1.0 + ((i - 0.5) * delta) ** 2), 1, 1)(arange(n, dtype=double)).sum()
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
