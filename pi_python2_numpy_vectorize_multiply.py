#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm.  Use NumPy.
#
#  Copyright © 2008–2011 Russel Winder

import time
import numpy

def f ( i ) :
    x = ( i - 0.5 ) * delta
    return 1.0 / ( 1.0 + x * x )

if __name__ == '__main__' :
    n = 10000000 # 00
    delta = 1.0 / n
    startTime = time.time ( )
    function = numpy.vectorize ( f )
    pi = 4.0 * delta * function ( numpy.arange ( n , dtype = numpy.float ) ).sum ( )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python NumPy Vectorize Multiply pi = " + str ( pi ) )
    print ( "==== Python NumPy Vectorize Multiply iteration count = " + str ( n ) )
    print ( "==== Python NumPy Vectorize Multiply elapse = " + str ( elapseTime ) )
