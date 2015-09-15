#! /usr/bin/env python3

#  Calculation of π using quadrature. Make use of the concurrent.futures facilities that are new in Python 3.2.
#
#  Copyright © 2011–2013, 2015  Russel Winder

from concurrent.futures import ProcessPoolExecutor
from output import out
from time import time


def processSlice(id, sliceSize, delta):
    sum = 0.0
    for i in range(1 + id * sliceSize, (id + 1) * sliceSize + 1):
        x = (i - 0.5) * delta
        sum += 1.0 / (1.0 + x * x)
    return sum


def execute(processCount):
    n = 10000000  # 100 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    sliceSize = n // processCount
    with ProcessPoolExecutor(max_workers=processCount) as executor:
        results = [executor.submit(processSlice, i, sliceSize, delta) for i in range(processCount)]
        pi = 4.0 * delta * sum(item.result() for item in results)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime, processCount)


if __name__ == '__main__':
    execute(1)
    execute(2)
    execute(8)
    execute(32)
