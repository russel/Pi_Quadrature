#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using the python-csp package by Sarah Mount.
#
#  Copyright © 2010 Russel Winder

import time
import multiprocessing

from processSlice_cython import processSlice

from csp.cspprocess import *

@process
def calculator ( channel , id , sliceSize , delta , _process = None ) :
    channel.write ( processSlice ( id , sliceSize , delta ) )
        
@process
def accumulator ( channel , n , delta , startTime , processCount , _process = None ) :
    pi = 4.0 * sum ( [ channel.read ( ) for i in xrange ( 0 , processCount ) ] ) * delta
    elapseTime = time.time ( ) - startTime
    print "==== Python CSP Single Cython Extension pi =" , pi
    print "==== Python CSP Single Cython Extension iteration count =", n
    print "==== Python CSP Single Cython Extension elapse =" , elapseTime
    print "==== Python CSP Single Cython Extension process count = ", processCount
    print "==== Python CSP Single Cython Extension processor count =" , multiprocessing.cpu_count ( )

def execute ( processCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n / processCount
    channel = Channel ( )
    processes = [ ] 
    for i in xrange ( 0 , processCount ) : processes.append ( calculator ( channel , i , sliceSize , delta ) )
    processes.append ( accumulator ( channel , n , delta , startTime , processCount ) )
    Par ( *processes ).start ( )

if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
