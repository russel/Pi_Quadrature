#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using the python-csp package by Sarah Mount.
#
#  Copyright © 2009 Russel Winder

import time
import multiprocessing

from csp.cspprocess import *

@process
def calculator ( channel , start , end , delta , _process = None ) :
    sum = 0.0
    for i in xrange ( start , end + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    channel.write ( sum )
        
@process
def accumulator ( channels , n , delta , startTime , processCount , _process = None ) :
    pi = 4.0 * sum ( [ channel.read ( ) for channel in channels ] ) * delta
    elapseTime = time.time ( ) - startTime
    print "==== Python CSP pi =" , pi
    print "==== Python CSP iteration count =", n
    print "==== Python CSP elapse =" , elapseTime
    print "==== Python CSP process count = ", processCount
    print "==== Python CSP processor count =" , multiprocessing.cpu_count ( )

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    slice = n / processCount
    channels = [ ]
    processes = [ ] 
    for i in range ( 0 , processCount ) :
        channel = Channel ( )
        channels.append ( channel )
        processes.append ( calculator ( channel , 1 + i * slice , ( i + 1 ) * slice , delta ) )
    processes.append ( accumulator ( channels , n , delta , startTime , processCount ) )
    Par ( *processes ).start ( )

if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
