#! /usr/bin/env python3

#  Calculation of π using quadrature. This is an SPMD, Python with Cython extension realization using
#  OpenMPI under the mpi4py package that provides Python binding to MPI.
#
#  Copyright © 2010–2014 Russel Winder

try:
    from mpi4py import MPI
except:
    from openmpi.mpi4py import MPI
from numpy import array
from output import out
from time import time

from processSlice_cython import processSlice

if __name__ == '__main__':
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    comm = MPI.COMM_WORLD
    myId = comm.Get_rank()
    sliceSize = n // comm.Get_size()
    localSum = array([processSlice(myId, sliceSize, delta)])
    sum = array([0.0])
    comm.Reduce((localSum, MPI.DOUBLE), (sum, MPI.DOUBLE))
    if myId == 0:
        pi = 4.0 * delta * sum[0]
        elapseTime = time() - startTime
        out(__file__, pi, n, elapseTime)
