#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  A Python program to calculate Pi using quadrature. This is an SPMD realization using OpenMPI under the
#  mpi4py package that provides Python binding to MPI.
#
#  Copyright © 2010–2012 Russel Winder

from mpi4py import MPI
from numpy import array
from output import out
from time import time


if __name__ == '__main__' :
  n = 10000000 # 100 times fewer than C due to speed issues.
  delta = 1.0 / n
  startTime = time ( )
  comm = MPI.COMM_WORLD
  nProcessors = comm.Get_size ( )
  myId = comm.Get_rank ( )
  sliceSize = n / nProcessors
  localSum = array ( [ 0.0 ] )
  for i in xrange ( 1 + myId * sliceSize , ( myId + 1 ) * sliceSize ) :
    x = ( i - 0.5 ) * delta
    localSum[0] += 1.0 / ( 1.0 + x * x )
  sum = array ( [ 0.0 ] )
  comm.Reduce ( ( localSum , MPI.DOUBLE ) , ( sum , MPI.DOUBLE ) )
  if myId == 0 :
    pi = 4.0 * delta * sum[0]
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime , nProcessors )
