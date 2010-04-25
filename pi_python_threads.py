#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using threads -- but this gives no parallelism because of the GIL.
#
#  Copyright © 2008-9 Russel Winder

import time
import threading
import Queue

def processSlice ( id , sliceSize , delta ) :
    sum = 0.0
    for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    results.append ( sum )

def execute ( threadCount ) :
    n = 100000000 # 10 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n / threadCount
    global results
    results = [ ]
    threads = [ threading.Thread ( target = processSlice , args = ( i , sliceSize , delta ) ) for i in xrange ( 0 , threadCount ) ]
    for thread in threads : thread.start ( )
    for thread in threads : thread.join ( )
    pi =  4.0 * sum ( results ) * delta
    elapseTime = time.time ( ) - startTime
    print "==== Python Threads pi =" , pi
    print "==== Python Threads iteration count =" , n
    print "==== Python Threads elapse =" , elapseTime
    print "==== Python Threads thread count = ", threadCount

if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
