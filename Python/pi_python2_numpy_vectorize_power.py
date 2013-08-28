#! /usr/bin/env python
# -*- coding:utf-8; -*-

#  Calculation of π using quadrature. Sequential algorithm using NumPy vectorize.
#
#  Copyright © 2008–2013  Russel Winder

from numpy import arange, vectorize, double
from output import out
from time import time

if __name__ == '__main__':
    n = 10000000  # 100 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * vectorize(lambda i: 1.0 / (1.0 + ((i - 0.5) * delta) ** 2))(arange(n, dtype=double)).sum()
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
