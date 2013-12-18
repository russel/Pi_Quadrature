#! /usr/bin/env python
# -*- coding:utf-8; -*-

#  Calculation of π using quadrature. This is an SPMD, Python with C++ extension via cffi realization using
#  OpenMPI under the mpi4py package that provides Python binding to MPI.
#
#  Copyright © 2010–2012 Russel Winder

from mpi4py import MPI
from numpy import array
from output import out
from time import time

from cffi import FFI

if __name__ == '__main__':
    ffi = FFI()
    ffi.cdef('double processSlice(int, int, double);')
    processSliceModule = ffi.dlopen('processSlice_cpp.so')
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    comm = MPI.COMM_WORLD
    myId = comm.Get_rank()
    sliceSize = n / comm.Get_size()
    localSum = array([processSliceModule.processSlice(myId, sliceSize, delta)])
    sum = array([0.0])
    comm.Reduce((localSum, MPI.DOUBLE), (sum, MPI.DOUBLE))
    if myId == 0:
        pi = 4.0 * delta * sum[0]
        elapseTime = time() - startTime
        out(__file__, pi, n, elapseTime)
