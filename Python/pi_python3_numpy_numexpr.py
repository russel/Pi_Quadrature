#! /usr/bin/env python3

#  Calculation of π using quadrature. Sequential algorithm using Numexpr evaluate.
#
#  Copyright © 2013 Russel Winder

from numexpr import evaluate, set_num_threads
from numpy import arange, double
from output import out
from time import time


def execute(threadCount):
    n = 100000000  # 10 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    set_num_threads(threadCount)
    value = arange(n, dtype=double)
    pi = 4.0 * delta * evaluate('1.0 / (1.0 + ((value - 0.5) * delta) ** 2)').sum()
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime, threadCount)


if __name__ == '__main__':
    execute(1)
    execute(2)
    execute(8)
    execute(32)
