#! /usr/bin/env python3

#  Calculation of Pi using quadrature.  Using the python-csp package by Sarah Mount.
#
#  Copyright © 2009–2011 Russel Winder

import time
import multiprocessing

from csp.os_process import process , Channel , Par

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time.time ( )
    slice = n // processCount
    channel = Channel ( )
    @process
    def accumulator ( ) :
        pi = 4.0 * delta * sum ( [ channel.read ( ) for i in range ( 0 , processCount ) ] )
        elapseTime = time.time ( ) - startTime
        print ( "==== Python CSP Single NestedDeep pi = " + str ( pi ) )
        print ( "==== Python CSP Single NestedDeep iteration count = " + str ( n ) )
        print ( "==== Python CSP Single NestedDeep elapse = " + str ( elapseTime ) )
        print ( "==== Python CSP Single NestedDeep process count = " + str ( processCount ) )
        print ( "==== Python CSP Single NestedDeep processor count = " + str ( multiprocessing.cpu_count ( ) ) )
    processes = [ ] 
    for i in range ( 0 , processCount ) :
        @process
        def calculator ( ) :
            sum = 0.0
            for j in range ( 1 + i * slice , ( i + 1 ) * slice ) :
                x = ( j - 0.5 ) * delta
                sum += 1.0 / ( 1.0 + x * x )
            channel.write ( sum )
        processes.append ( calculator ( ) )
    processes.append ( accumulator ( ) )
    Par ( *processes ).start ( )

if __name__ == '__main__' :
    execute ( 1 )
    print ( )
    execute ( 2 )
    print ( )
    execute ( 8 )
    print ( )
    execute ( 32 )
