#! /usr/bin/env python
# -*- coding:utf-8; -*-

#  Calculation of π using quadrature. Uses threads and an extension. ctypes is careful to release the GIL
#  whilst C code is running so we get real parallelism.
#
#  Copyright © 2008–2012 Russel Winder

from output import out
from Queue import Queue
from threading import Thread
from time import time

import ctypes

def processSlice(id, sliceSize, delta, results):
    results.put(processSliceModule.processSlice(id, sliceSize, delta))

def execute(threadCount):
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sliceSize = n / threadCount
    results = Queue(threadCount)
    threads = [Thread(target=processSlice, args=(i, sliceSize, delta, results)) for i in xrange(0, threadCount)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join()
    pi = 4.0 * delta * sum([results.get() for i in xrange(threadCount)])
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime, threadCount)

if __name__ == '__main__':
    processSliceModule = ctypes.cdll.LoadLibrary('processSlice_c.so')
    processSliceModule.processSlice.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_double]
    processSliceModule.processSlice.restype = ctypes.c_double
    execute(1)
    execute(2)
    execute(8)
    execute(32)
