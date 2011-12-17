#! /usr/bin/env python3

#  Calculation of Pi using quadrature.  Using the python-csp package by Sarah Mount.
#
#  Copyright © 2010–2011 Russel Winder

import time
import multiprocessing

from processSlice_cython_py3 import processSlice

from csp.os_process import process , Channel , Par

@process
def calculator ( channel , id , sliceSize , delta ) :
    channel.write ( processSlice ( id , sliceSize , delta ) )
        
@process
def accumulator ( channel , n , delta , startTime , processCount ) :
    pi = 4.0 * delta * sum ( [ channel.read ( ) for i in range ( 0 , processCount ) ] )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python CSP Single Cython Extension pi = " + str ( pi ) )
    print ( "==== Python CSP Single Cython Extension iteration count = " + str ( n ) )
    print ( "==== Python CSP Single Cython Extension elapse = " + str ( elapseTime ) )
    print ( "==== Python CSP Single Cython Extension process count = " + str ( processCount ) )
    print ( "==== Python CSP Single Cython Extension processor count = " + str ( multiprocessing.cpu_count ( ) ) )

def execute ( processCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time.time ( )
    sliceSize = n // processCount
    channel = Channel ( )
    processes = [ ] 
    for i in range ( 0 , processCount ) : processes.append ( calculator ( channel , i , sliceSize , delta ) )
    processes.append ( accumulator ( channel , n , delta , startTime , processCount ) )
    Par ( *processes ).start ( )

if __name__ == '__main__' :
    execute ( 1 )
    print ( )
    execute ( 2 )
    print ( )
    execute ( 8 )
    print ( )
    execute ( 32 )
