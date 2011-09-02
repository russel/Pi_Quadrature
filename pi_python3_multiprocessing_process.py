#! /usr/bin/env python3
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using the multiprocessing package with processes sending messages to
#  a collecting queue.
#
#  Copyright © 2008-11 Russel Winder

import time
import multiprocessing

def processSlice ( id , sliceSize , delta , output ) :
    sum = 0.0
    for i in range ( 1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    output.put ( sum )
    output.close ( )

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n // processCount
    resultsQueue = multiprocessing.Queue ( )
    processes = [ multiprocessing.Process ( target = processSlice , args = ( i , sliceSize , delta , resultsQueue ) ) for i in range ( 0 , processCount ) ]
    for p in processes : p.start ( )
    # for p in processes : p.join ( ) # Do not need to syncronize the processes, the get will do that by blocking.
    results = [ resultsQueue.get ( ) for i in range ( 0 , processCount ) ]
    pi = 4.0 * sum ( results ) * delta
    elapseTime = time.time ( ) - startTime
    print ( "==== Python Multiprocessing Process pi = " + str ( pi ) )
    print ( "==== Python Multiprocessing Process iteration count = "+ str ( n ) )
    print ( "==== Python Multiprocessing Process elapse = " + str ( elapseTime ) )
    print ( "==== Python Multiprocessing Process process count = "+ str ( processCount ) )
    print ( "==== Python Multiprocessing Process processor count = " + str ( multiprocessing.cpu_count ( ) ) )
    print ( )

if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
