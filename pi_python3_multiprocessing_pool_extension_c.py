#! /usr/bin/env python3

#  Calculation of Pi using quadrature. Using the multiprocessing package to provide a process pool to enable
#  asynchronous function calls very akin to futures.
#
#  Copyright © 2008–2012 Russel Winder

import time
import multiprocessing
import ctypes

#  Cannot call the C function directly in the executor submit call hence this function.

def processSlice ( id , sliceSize , delta ) :
    return processSliceModule.processSlice ( id , sliceSize , delta )

def execute ( processCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n // processCount
    pool = multiprocessing.Pool ( processes = processCount )
    results = [ pool.apply_async ( processSlice , args = ( i , sliceSize , delta ) ) for i in range ( 0 , processCount ) ]
    pool.close ( )
    pi = 4.0 * delta * sum ( [ item.get ( ) for item in results ] )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python Multiprocessing Pool C Extension pi = " + str ( pi ) )
    print ( "==== Python Multiprocessing Pool C Extension iteration count = " + str ( n ) )
    print ( "==== Python Multiprocessing Pool C Extension elapse = " + str ( elapseTime ) )
    print ( "==== Python Multiprocessing Pool C Extension process count = " + str ( processCount ) )
    print ( "==== Python Multiprocessing Pool C Extension processor count = " + str ( multiprocessing.cpu_count ( ) ) )
    
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
