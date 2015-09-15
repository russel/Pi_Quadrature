#! /usr/bin/env python3

#  Calculation of π using quadrature. Parallel algorithm using D library.
#
#  Copyright © 2015  Russel Winder

from output import out
from time import time

from cffi import FFI

if __name__ == '__main__':
    ffi = FFI()
    ffi.cdef('double parallel(int, double);')
    processSliceModule = ffi.dlopen('processAll_library_cpp.so')
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    pi = processSliceModule.parallel(n, delta)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
