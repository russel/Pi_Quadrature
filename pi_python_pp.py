#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Use the Parallel Python package.
#
#  Copyright © 2008-10 Russel Winder

import time

import sys
import os
import pp

def processSlice ( id , sliceSize , delta ) :
    sum = 0.0
    for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    return sum

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n / processCount
    server = pp.Server ( )
    jobs = [ server.submit ( processSlice , ( i , sliceSize , delta ) ) for i in xrange ( 0, processCount ) ]
    results = [ job ( ) for job in jobs ]
    pi = 4.0 * sum ( results ) * delta
    elapseTime = time.time ( ) - startTime
    print "==== Python PP pi =" , pi
    print "==== Python PP iteration count =", n
    print "==== Python PP elapse =" , elapseTime
    print "==== Python PP process count =" , processCount
    print "==== Python PP processor count =" , server.get_ncpus ( )
    server.print_stats ( )
    
if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
