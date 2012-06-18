#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Using the multiprocessing package to provide a process pool to enable
#  asynchronous function calls very akin to futures..
#
#  Copyright © 2008–2012 Russel Winder

from multiprocessing import Pool, cpu_count
from output import out
from time import time

def processSlice ( id , sliceSize , delta ) :
    sum = 0.0
    for i in xrange (  1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    return sum

def execute ( processCount ) :
    n = 10000000 # 100 times fewer due to speed issues.
    delta = 1.0 / n
    startTime = time ( )
    sliceSize = n / processCount
    pool = Pool ( processes = processCount )
    results = [ pool.apply_async ( processSlice , args = ( i , sliceSize , delta ) ) for i in xrange ( 0 , processCount ) ]
    #pool.close ( )
    pi = 4.0 * delta * sum ( [ item.get ( ) for item in results ] )
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime , processCount , cpu_count ( ) )

if __name__ == '__main__' :
    execute ( 1 )
    execute ( 2 )
    execute ( 8 )
    execute ( 32 )
