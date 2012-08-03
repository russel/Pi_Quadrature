#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Using the pycsp package byRune Møllegaard Friborg.
#
#  Copyright © 2009–2012 Russel Winder

from output import out
from pycsp.processes import process , Channel , Parallel
from time import time

@process
def calculator ( channel , id , sliceSize , delta ) :
    sum = 0.0
    for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
        x = ( i - 0.5 ) * delta
        sum += 1.0 / ( 1.0 + x * x )
    channel ( sum )
        
@process
def accumulator ( channel , n , delta , startTime , processCount ) :
    pi = 4.0 * delta * sum ( [ channel ( ) for i in xrange ( 0 , processCount ) ] )
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime , processCount )

def execute ( processCount ) :
    n = 10000000 # 100 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time ( )
    sliceSize = n / processCount
    channel = Channel ( )
    processes = [ calculator ( -channel , i , sliceSize , delta )  for i in xrange ( 0 , processCount ) ]
    processes.append ( accumulator ( +channel , n , delta , startTime , processCount ) )
    Parallel ( *processes )

if __name__ == '__main__' :
    execute ( 1 )
    execute ( 2 )
    execute ( 8 )
    execute ( 32 )
