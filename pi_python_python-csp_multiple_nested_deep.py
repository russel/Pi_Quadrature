#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using the python-csp package by Sarah Mount.
#
#  Copyright © 2009-10 Russel Winder

import time
import multiprocessing

from csp.cspprocess import *

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n / processCount
    channels = [ ]
    @process
    def accumulator ( _process = None ) :
        pi = 4.0 * sum ( [ channel.read ( ) for channel in channels ] ) * delta
        elapseTime = time.time ( ) - startTime
        print "==== Python CSP Multiple NestedDeep pi =" , pi
        print "==== Python CSP Multiple NestedDeep iteration count =", n
        print "==== Python CSP Multiple NestedDeep elapse =" , elapseTime
        print "==== Python CSP Multiple NestedDeep process count = ", processCount
        print "==== Python CSP Multiple NestedDeep processor count =" , multiprocessing.cpu_count ( )
    processes = [ ] 
    for i in range ( 0 , processCount ) :
        channel = Channel ( )
        channels.append ( channel )
        @process
        def calculator ( id , _process = None ) :
            sum = 0.0
            for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize ) :
                x = ( i - 0.5 ) * delta
                sum += 1.0 / ( 1.0 + x * x )
            channels[id].write ( sum )
        processes.append ( calculator ( i ) )
    processes.append ( accumulator ( ) )
    Par ( *processes ).start ( )

if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
