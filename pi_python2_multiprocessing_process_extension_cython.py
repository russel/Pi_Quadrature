#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Using the multiprocessing package with processes sending messages to
#  a collecting queue.
#
#  Copyright © 2008–2012 Russel Winder

from multiprocessing import Queue , Process
from output import out
from time import time

from processSlice_cython_py2 import processSlice

def calculator ( id , sliceSize , delta , output ) :
    output.put ( processSlice ( id , sliceSize , delta ) )
    output.close ( )

def execute ( processCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time ( )
    sliceSize = n / processCount
    resultsQueue = Queue ( )
    processes = [ Process ( target = calculator , args = ( i , sliceSize , delta , resultsQueue ) ) for i in range ( 0 , processCount ) ]
    for p in processes : p.start ( )
    pi = 4.0 * delta * sum ( [ resultsQueue.get ( ) for i in range ( 0 , processCount ) ] )
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime , processCount )

if __name__ == '__main__' :
    execute ( 1 )
    execute ( 2 )
    execute ( 8 )
    execute ( 32 )
