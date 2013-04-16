#! /usr/bin/env python
# -*- coding:utf-8; -*-

#  Calculation of π using quadrature. Uses the multiprocessing package to provide a process pool to enable
#  asynchronous function calls very akin to futures.
#
#  Copyright © 2008–2013 Russel Winder

from multiprocessing import Pool
from output import out
from time import time

from processSlice_cython_py2 import processSlice

def execute(processCount):
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sliceSize = n / processCount
    pool = Pool(processes=processCount)
    results = [pool.apply_async(processSlice, args=(i, sliceSize, delta)) for i in range(0, processCount)]
    pi = 4.0 * delta * sum(item.get() for item in results)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime, processCount)

if __name__ == '__main__':
    execute(1)
    execute(2)
    execute(8)
    execute(32)
