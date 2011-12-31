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
    n = 100000000 # 0
    delta = 1.0 / n
    startTime = time.time ( )
    pi = 4.0 * delta * numpy.fromfunction ( f , ( n , ) , dtype = numpy.float ).sum ( )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python NumPy FromFunction Multiply pi = " + str ( pi ) )
    print ( "==== Python NumPy FromFunction Multiply iteration count = " + str ( n ) )
    print ( "==== Python NumPy FromFunction Multiply elapse = " + str ( elapseTime ) )
