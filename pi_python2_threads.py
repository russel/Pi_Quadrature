#! /usr/bin/env python2
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using threads -- but this gives no parallelism because of the GIL.
#  If the append operation on a list is atomic then we can just use a list as the mechanism for receiving
#  the results.  However it is not clear that that operation is atomic so we use a Queue as the way of
#  receiving results since that has the necessary guarantees to be thread-safe.
#
#  Copyright © 2008-10 Russel Winder

import time
import threading
import Queue

def processSlice ( id , sliceSize , delta ) :
    sum = 0.0
    for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    results.put ( sum )

def execute ( threadCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n // threadCount
    global results
    results = Queue.Queue ( threadCount )
    threads = [ threading.Thread ( target = processSlice , args = ( i , sliceSize , delta ) ) for i in xrange ( 0 , threadCount ) ]
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
