#! /usr/bin/env python3

#  Calculation of Pi using quadrature. Using the concurrent.futures facilities.
#
#  Copyright © 2011 Russel Winder

import time
import multiprocessing

import concurrent.futures #  New in Python 3.2

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
    with concurrent.futures.ProcessPoolExecutor ( max_workers = processCount ) as executor :
        results = [ executor.submit ( processSlice , i , sliceSize , delta ) for i in range ( processCount ) ]
    pi = 4.0 * delta * sum ( [ item.result ( ) for item in results ] )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python Concurrent Futures pi = " + str ( pi ) )
    print ( "==== Python Concurrent Futures iteration count = " + str ( n ) )
    print ( "==== Python Concurrent Futures elapse = " + str ( elapseTime ) )
    print ( "==== Python Concurrent Futures process count = " + str ( processCount ) )
    print ( "==== Python Concurrent Futures processor count = " + str ( multiprocessing.cpu_count ( ) ) )

if __name__ == '__main__' :
    execute ( 1 )
    print ( )
    execute ( 2 )
    print ( )
    execute ( 8 )
    print ( )
    execute ( 32 )
