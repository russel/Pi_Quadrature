#! /usr/bin/env python3

#  Calculation of Pi using quadrature.  Using threads and extensions.  ctypes is careful to release the GIL
#  whilst C code is running so we get real parallelism. We use a Queue as the way of receiving results since
#  that has the necessary guarantees to be thread-safe.
#
#  Copyright © 2008–2012 Russel Winder

from output import out
from queue import Queue
from threading import Thread
from time import time

from processSlice_cython_py3 import processSlice

def calculator ( id , sliceSize , delta , results ) :
    results.put ( processSlice ( id , sliceSize , delta ) )

def execute ( threadCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time ( )
    sliceSize = n // threadCount
    results = Queue ( threadCount )
    threads = [ Thread ( target = calculator , args = ( i , sliceSize , delta , results ) ) for i in range ( 0 , threadCount ) ]
    for thread in threads : thread.start ( )
    for thread in threads : thread.join ( )
    pi =  4.0 * delta * sum ( [ results.get ( ) for i in range ( threadCount ) ] )
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime , threadCount )

if __name__ == '__main__' :
    execute ( 1 )
    execute ( 2 )
    execute ( 8 )
    execute ( 32 )
