#! /usr/bin/env python3

#  Calculation of Pi using quadrature. Using the concurrent.futures facilities.
#
#  Copyright © 2011–2012 Russel Winder

import time
import multiprocessing
import concurrent.futures #  New in Python 3.2
import ctypes

#  Cannot call the C function directly in the executor submit call hence this function.

def processSlice ( id , sliceSize , delta ) :
    return processSliceModule.processSlice ( id , sliceSize , delta )

def execute ( processCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n // processCount
    with concurrent.futures.ProcessPoolExecutor ( max_workers = processCount ) as executor :
        results = [ executor.submit ( processSlice , i , sliceSize , delta ) for i in range ( processCount ) ]
    pi = 4.0 * delta * sum ( [ item.result ( ) for item in results ] )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python Concurrent Futures C Extension pi = " + str ( pi ) )
    print ( "==== Python Concurrent Futures C Extension iteration count = " + str ( n ) )
    print ( "==== Python Concurrent Futures C Extension elapse = " + str ( elapseTime ) )
    print ( "==== Python Concurrent Futures C Extension process count = " + str ( processCount ) )
    print ( "==== Python Concurrent Futures C Extension processor count = " + str ( multiprocessing.cpu_count ( ) ) )

if __name__ == '__main__' :
    processSliceModule = ctypes.cdll.LoadLibrary ( 'processSlice_c.so' )
    processSliceModule.processSlice.argtypes = [ ctypes.c_int , ctypes.c_int , ctypes.c_double ]
    processSliceModule.processSlice.restype = ctypes.c_double
    execute ( 1 )
    print ( )
    execute ( 2 )
    print ( )
    execute ( 8 )
    print ( )
    execute ( 32 )
