#! /usr/bin/env python3

#  Calculation of π using quadrature. Make use of the concurrent.futures facilities that are new in Python 3.2.
#
#  Copyright © 2011–2015  Russel Winder

from concurrent.futures import ProcessPoolExecutor
from output import out
from time import time

import ctypes

#  Cannot call the C function directly in the executor submit call.

def processSlice(id, sliceSize, delta):
    return processSliceModule.processSlice(id, sliceSize, delta)

def execute(processCount):
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sliceSize = n // processCount
    with ProcessPoolExecutor(max_workers=processCount) as executor:
        results = [executor.submit(processSlice, i, sliceSize, delta) for i in range(processCount)]
        pi = 4.0 * delta * sum(item.result() for item in results)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime, processCount)

if __name__ == '__main__':
    processSliceModule = ctypes.cdll.LoadLibrary('processSlice_d.so')
    processSliceModule.processSlice.argtypes = [ctypes.c_int, ctypes.c_int, ctypes.c_double]
    processSliceModule.processSlice.restype = ctypes.c_double
    execute(1)
    execute(2)
    execute(8)
    execute(32)
