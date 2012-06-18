#! /usr/bin/env python3

#  Calculation of Pi using quadrature. Using the concurrent.futures facilities that are new in Python 3.2.
#
#  Copyright © 2011–2012 Russel Winder

from concurrent.futures import ProcessPoolExecutor
from multiprocessing import cpu_count
from output import out
from time import time

from processSlice_cython_py3 import processSlice

def execute ( processCount ) :
    n = 1000000000
    delta = 1.0 / n
    startTime = time ( )
    sliceSize = n // processCount
    with ProcessPoolExecutor ( max_workers = processCount ) as executor :
        results = [ executor.submit ( processSlice , i , sliceSize , delta ) for i in range ( processCount ) ]
    pi = 4.0 * delta * sum ( [ item.result ( ) for item in results ] )
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime , processCount , cpu_count ( ) )

if __name__ == '__main__' :
    execute ( 1 )
    execute ( 2 )
    execute ( 8 )
    execute ( 32 )
