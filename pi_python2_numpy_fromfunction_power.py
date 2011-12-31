#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm.  Use NumPy.
#
#  Copyright © 2008–2011 Russel Winder

import time
import numpy

if __name__ == '__main__' :
    n = 100000000 # 0
    delta = 1.0 / n
    startTime = time.time ( )
    pi = 4.0 * delta * numpy.fromfunction ( lambda i : 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 ) , ( n , ) , dtype = numpy.float ).sum ( )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python NumPy FromFunction Power pi = " + str ( pi ) )
    print ( "==== Python NumPy FromFunction Power iteration count = " + str ( n ) )
    print ( "==== Python NumPy FromFunction Power elapse = " + str ( elapseTime ) )
