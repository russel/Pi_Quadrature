#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using the python-csp package by Sarah Mount.
#
#  Copyright © 2010 Russel Winder

import time
import multiprocessing
import ctypes

from csp.cspprocess import *

@process
def calculator ( channel , id , sliceSize , delta , _process = None ) :
    processSlice = ctypes.cdll.LoadLibrary ( 'lib_processSlice_c.so' )
    processSlice.processSlice.argtypes = [ ctypes.c_long , ctypes.c_long , ctypes.c_double ]
    processSlice.processSlice.restype = ctypes.c_double
    channel.write ( processSlice.processSlice ( id , sliceSize , delta ) )
        
@process
def accumulator ( channel , n , delta , startTime , processCount , _process = None ) :
    pi = 4.0 * sum ( [ channel.read ( ) for i in xrange ( 0 , processCount ) ] ) * delta
    elapseTime = time.time ( ) - startTime
    print "==== Python CSP Single C pi =" , pi
    print "==== Python CSP Single C iteration count =", n
    print "==== Python CSP Single C elapse =" , elapseTime
    print "==== Python CSP Single C process count = ", processCount
    print "==== Python CSP Single C processor count =" , multiprocessing.cpu_count ( )

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
