#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  A Python program to calculate Pi using quadrature.  This is an SPMD realization using OpenMPI under the
#  mpi4py package that provides Python binding to MPI.
#
#  Copyright © 2010–2012 Russel Winder

import time
from mpi4py import MPI
import numpy
import ctypes

if __name__ == '__main__' :
  processSliceModule = ctypes.cdll.LoadLibrary ( 'processSlice_c.so' )
  processSliceModule.processSlice.argtypes = [ ctypes.c_int , ctypes.c_int , ctypes.c_double ]
  processSliceModule.processSlice.restype = ctypes.c_double

  n = 1000000000
  delta = 1.0 / n
  startTime = time.time ( )
  comm = MPI.COMM_WORLD
  nProcessors = comm.Get_size ( )
  myId = comm.Get_rank ( )
  sliceSize = n / nProcessors
  localSum = numpy.array ( [ processSliceModule.processSlice ( myId , sliceSize , delta ) ] )
  sum = numpy.array ( [ 0.0 ] )
  comm.Reduce ( ( localSum , MPI.DOUBLE ) , ( sum , MPI.DOUBLE ) )
  if myId == 0 :
    pi = 4.0 * delta * sum[0]
    elapseTime = time.time ( ) - startTime
    print ( "==== Python MPI C Extension pi = " + str ( pi ) )
    print ( "==== Python MPI C Extension iteration count = " + str ( n ) )
    print ( "==== Python MPI C Extension elapse = " + str ( elapseTime ) )
    print ( "==== Python MPI C Extension processorCount = " + str ( nProcessors ) )
