#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature.  Using the python-csp package by Sarah Mount.
#
#  Copyright © 2010–2011 Russel Winder

import time
import multiprocessing

from processSlice_pyrex_py2 import processSlice

from csp.os_process import process , Channel , Par

@process
def calculator ( channel , id , sliceSize , delta ) :
    channel.write ( processSlice ( id , sliceSize , delta ) )
        
@process
def accumulator ( channel , n , delta , startTime , processCount ) :
    pi = 4.0 * sum ( [ channel.read ( ) for i in xrange ( 0 , processCount ) ] ) * delta
    elapseTime = time.time ( ) - startTime
    print ( "==== Python CSP Single Pyrex Extension pi = " + str ( pi ) )
    print ( "==== Python CSP Single Pyrex Extension iteration count = "+ str ( n ) )
    print ( "==== Python CSP Single Pyrex Extension elapse = " + str ( elapseTime ) )
    print ( "==== Python CSP Single Pyrex Extension process count = "+ str ( processCount ) )
    print ( "==== Python CSP Single Pyrex Extension processor count = " + str ( multiprocessing.cpu_count ( ) ) )

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
