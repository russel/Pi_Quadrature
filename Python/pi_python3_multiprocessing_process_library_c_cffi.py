#! /usr/bin/env python3

#  Calculation of π using quadrature. Uses the multiprocessing package with processes sending messages to
#  a collecting queue.
#
#  Copyright © 2008–2013 Russel Winder

from multiprocessing import Queue, Process
from output import out
from time import time

from cffi import FFI


def processSlice(id, sliceSize, delta, output):
    output.put(processSliceModule.processSlice(id, sliceSize, delta))


def execute(processCount):
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sliceSize = n // processCount
    resultsQueue = Queue()
    processes = [Process(target=processSlice, args=(i, sliceSize, delta, resultsQueue)) for i in range(0, processCount)]
    for p in processes:
        p.start()
    pi = 4.0 * delta * sum(resultsQueue.get() for i in range(0, processCount))
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime, processCount)


if __name__ == '__main__':
    ffi = FFI()
    ffi.cdef('double processSlice(int, int, double);')
    processSliceModule = ffi.dlopen('processSlice_library_c.so')
    execute(1)
    execute(2)
    execute(8)
    execute(32)
