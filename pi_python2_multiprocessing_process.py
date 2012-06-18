#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Using the multiprocessing package with processes sending messages to
#  a collecting queue.
#
#  Copyright © 2008–2012 Russel Winder

from multiprocessing import Queue , Process, cpu_count
from output import out
from time import time

def processSlice ( id , sliceSize , delta , output ) :
    sum = 0.0
    for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    output.put ( sum )
    output.close ( )

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time ( )
    sliceSize = n / processCount
    resultsQueue = Queue ( )
    processes = [ Process ( target = processSlice , args = ( i , sliceSize , delta , resultsQueue ) ) for i in xrange ( 0 , processCount ) ]
    for p in processes : p.start ( )
    pi = 4.0 * delta * sum ( [ resultsQueue.get ( ) for i in xrange ( 0 , processCount ) ] )
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime , processCount , cpu_count ( ) )

if __name__ == '__main__' :
    execute ( 1 )
    execute ( 2 )
    execute ( 8 )
    execute ( 32 )
