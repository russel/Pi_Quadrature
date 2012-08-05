#! /usr/bin/env python
# -*- coding:utf-8; -*-

#  Calculation of π using quadrature. Sequential algorithm using a while loop.
#
#  Copyright © 2008–2012 Russel Winder

from output import out
from time import time

if __name__ == '__main__':
    n = 10000000  # 100 times fewer than C due to speed issues
    delta = 1.0 / n
    startTime = time()
    sum = 0.0
    i = 1
    while i < n + 1:
        sum += 1.0 / (1.0 + ((i - 0.5) * delta) ** 2)
        i += 1
    pi = 4.0 * delta * sum
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
