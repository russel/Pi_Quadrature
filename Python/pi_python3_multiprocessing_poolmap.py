#! /usr/bin/env python3

#  Calculation of π using quadrature. Uses the multiprocessing package to provide a process pool to enable
#  asynchronous function calls very akin to futures.
#
#  Copyright © 2008–2013, 2015 Russel Winder

from multiprocessing import Pool
from functools import partial
from output import out
from time import time


def f(i, delta):
    x = (i - 0.5) * delta
    return 1.0 / (1.0 + x * x)


def execute(processCount):
    n = 10000000  # 100 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    with Pool(processes=processCount) as pool:
        pi = 4.0 * delta * sum(pool.map(partial(f, delta=delta), range(1, n)))
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime, processCount)


if __name__ == '__main__':
    execute(1)
    execute(2)
    execute(8)
    execute(32)
