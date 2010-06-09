#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Using mpi4py.
#
#  Copyright © 2010 Russel Winder

import time
from mpi4py import MPI
import numpy

n = 10000000 # 100 times fewer due to speed issues.
delta = 1.0 / n
startTime = time.time ( )
comm = MPI.COMM_WORLD
nProcessors = comm.Get_size ( )
myId = comm.Get_rank ( )
sliceSize = n / nProcessors
localSum = numpy.array ( [ 0.0 ] )
for i in xrange ( 1 + myId * sliceSize , ( myId + 1 ) * sliceSize ) :
  x = ( i - 0.5 ) * delta
  localSum[0] += 1.0 / ( 1.0 + x * x )
sum = numpy.array ( [ 0.0 ] )
comm.Reduce ( ( localSum , MPI.DOUBLE ) , ( sum , MPI.DOUBLE ) )
if myId == 0 :
    pi = 4.0 * sum[0] * delta
    elapseTime = time.time ( ) - startTime
    print ( "==== Python MPI pi = " + str ( pi ) )
    print ( "==== Python MPI iteration count = " + str ( n ) )
    print ( "==== Python MPI elapse = " + str ( elapseTime ) )
    print ( "==== Python MPI processorCount = " + str ( nProcessors ) )
