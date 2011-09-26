#! /usr/bin/env python3

#  Calculation of Pi using quadrature. Using the multiprocessing package to provide a process pool to enable
#  asynchronous function calls very akin to futures.
#
#  Copyright © 2008–2011 Russel Winder

import time
import multiprocessing

def processSlice ( id , sliceSize , delta ) :
    sum = 0.0
    for i in range (  1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    return sum

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n // processCount
    pool = multiprocessing.Pool ( processes = processCount )
    results = [ pool.apply_async ( processSlice , args = ( i , sliceSize , delta ) ) for i in range ( 0 , processCount ) ]
    pool.close ( )
    # pool.join ( ) # Do not need to syncronize the processes, the get will do that by blocking.
    results = [ item.get ( ) for item in results ]
    pi = 4.0 * sum ( results ) * delta
    elapseTime = time.time ( ) - startTime
    print ( "==== Python Multiprocessing Pool pi = " + str ( pi ) )
    print ( "==== Python Multiprocessing Pool iteration count = "+ str ( n ) )
    print ( "==== Python Multiprocessing Pool elapse = " + str ( elapseTime ) )
    print ( "==== Python Multiprocessing Pool process count = " + str ( processCount ) )
    print ( "==== Python Multiprocessing Pool processor count = " + str ( multiprocessing.cpu_count ( ) ) )
    print ( )
    
if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
