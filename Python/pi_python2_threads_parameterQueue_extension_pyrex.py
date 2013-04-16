#! /usr/bin/env python
# -*- coding:utf-8; -*-

#  Calculation of π using quadrature. Uses threads and a Pyrex extension. This circumvents the GIL and
#  allow for real parallelism.
#
#  Copyright © 2008–2013 Russel Winder

from output import out
from Queue import Queue
from threading import Thread
from time import time

from processSlice_pyrex_py2 import processSlice

def calculator(id, sliceSize, delta, results):
    results.put(processSlice(id, sliceSize, delta))

def execute(threadCount):
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sliceSize = n // threadCount
    results = Queue(threadCount)
    threads = [Thread(target=calculator, args=(i, sliceSize, delta, results)) for i in range(0, threadCount)]
    for thread in threads:
        thread.start()
    pi = 4.0 * delta * sum(results.get() for i in range(threadCount))
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime, threadCount)

if __name__ == '__main__':
    execute(1)
    execute(2)
    execute(8)
    execute(32)
