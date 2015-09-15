#! /usr/bin/env python3

#  Calculation of π using quadrature. Uses threads but this gives no parallelism because of the GIL.
#  This version passes a reference to a thread-safe list to each child to pass back results to the parent.
#
#  Copyright © 2008–2013 Russel Winder

from output import out
from queue import Queue
from threading import Thread
from time import time


def processSlice(id, sliceSize, delta, results):
    sum = 0.0
    for i in range(1 + id * sliceSize, (id + 1) * sliceSize + 1):
        x = (i - 0.5) * delta
        sum += 1.0 / (1.0 + x * x)
    results.put(sum)


def execute(threadCount):
    n = 10000000  # 100 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time()
    sliceSize = n // threadCount
    results = Queue(threadCount)
    threads = [Thread(target=processSlice, args=(i, sliceSize, delta, results)) for i in range(0, threadCount)]
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
