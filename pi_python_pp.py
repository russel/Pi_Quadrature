#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Use the Parallel Python package.
#
#  Copyright © 2008-9 Russel Winder

import time

#  python-pp only installs version 1.5.6 in Ubuntu Karmic so use my installation of 1.5.7 which has the
#  Python 2.5 compatibility.

import sys
import os
sys.path.insert ( 0 , os.environ['HOME'] + '/lib/Python/lib/python2.6/pp' )
import pp

def processSlice ( start , end , delta ) :
    sum = 0.0
    for i in xrange ( start , end + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    return sum

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    slice = n / processCount
    server = pp.Server ( )
    jobs = [ server.submit ( processSlice , ( 1 + i * slice , ( i + 1 ) * slice , delta ) ) for i in range ( 0, processCount ) ]
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
