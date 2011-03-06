#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using threads and extensions.  ctypes is careful to release the GIL
#  whilst C code is running so we get real parallelism. We use a Queue as the way of receiving results since
#  that has the necessary guarantees to be thread-safe.
#
#  Copyright © 2008--2011 Russel Winder

import time
import threading
import Queue
import ctypes

def processSlice ( id , sliceSize , delta , results ) :
    processSlice = ctypes.cdll.LoadLibrary ( 'processSlice_c.so' )
    processSlice.processSlice.argtypes = [ ctypes.c_int , ctypes.c_int , ctypes.c_double ]
    processSlice.processSlice.restype = ctypes.c_double
    results.put ( processSlice.processSlice ( id , sliceSize , delta ) )

def execute ( threadCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n / threadCount
    results = Queue.Queue ( threadCount )
    threads = [ threading.Thread ( target = processSlice , args = ( i , sliceSize , delta , results ) ) for i in xrange ( 0 , threadCount ) ]
    for thread in threads : thread.start ( )
    for thread in threads : thread.join ( )
    pi =  4.0 * sum ( [ results.get ( ) for i in xrange ( threadCount ) ] ) * delta
    elapseTime = time.time ( ) - startTime
    print ( "==== Python Threads pi = " + str ( pi ) )
    print ( "==== Python Threads iteration count = " + str ( n ) )
    print ( "==== Python Threads elapse = " + str ( elapseTime ) )
    print ( "==== Python Threads thread count = "+ str ( threadCount ) )

if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
