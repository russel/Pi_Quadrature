#! /usr/bin/env python3

#  Calculation of Pi using quadrature.  Using the multiprocessing package with processes sending messages to
#  a collecting queue.
#
#  Copyright © 2008–2012 Russel Winder

import time
import multiprocessing
import ctypes

def processSlice ( id , sliceSize , delta , output ) :
    output.put ( processSliceModule.processSlice ( id , sliceSize , delta ) )
    output.close ( )

def execute ( processCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n // processCount
    resultsQueue = multiprocessing.Queue ( )
    processes = [ multiprocessing.Process ( target = processSlice , args = ( i , sliceSize , delta , resultsQueue ) ) for i in range ( 0 , processCount ) ]
    for p in processes : p.start ( )
    pi = 4.0 * delta * sum ( [ resultsQueue.get ( ) for i in range ( 0 , processCount ) ] )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python Multiprocessing Process C Extension pi = " + str ( pi ) )
    print ( "==== Python Multiprocessing Process C Extension iteration count = " + str ( n ) )
    print ( "==== Python Multiprocessing Process C Extension elapse = " + str ( elapseTime ) )
    print ( "==== Python Multiprocessing Process C Extension process count = " + str ( processCount ) )
    print ( "==== Python Multiprocessing Process C Extension processor count = " + str ( multiprocessing.cpu_count ( ) ) )
    print ( )

if __name__ == '__main__' :
    processSliceModule = ctypes.cdll.LoadLibrary ( 'processSlice_c.so' )
    processSliceModule.processSlice.argtypes = [ ctypes.c_int , ctypes.c_int , ctypes.c_double ]
    processSliceModule.processSlice.restype = ctypes.c_double
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
