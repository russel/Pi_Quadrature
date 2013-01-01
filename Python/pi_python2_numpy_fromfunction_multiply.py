#! /usr/bin/env python
# -*- coding:utf-8; -*-

#  Calculation of π using quadrature. Sequential algorithm using NumPy fromfunction.
#
#  Copyright © 2008–2012 Russel Winder

from numpy import float, fromfunction
from output import out
from time import time

def f(i):
    x = (i - 0.5) * delta
    return 1.0 / (1.0 + x * x)

if __name__ == '__main__':
    n = 100000000  # 10 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * fromfunction(f, (n,), dtype=float).sum()
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
