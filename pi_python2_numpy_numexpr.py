#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm.  Use NumPy.
#
#  Copyright © 2008–2011 Russel Winder

import time
import numpy
import numexpr

def execute ( threadCount ) :
    n = 100000000 # 0
    delta = 1.0 / n
    startTime = time.time ( )
    numexpr.set_num_threads ( threadCount )
    value = numpy.arange ( n )
    pi = 4.0 * delta * numexpr.evaluate ( "1.0 / ( 1.0 + ( ( value - 0.5 ) * delta ) ** 2 )" ).sum ( )
    elapseTime = time.time ( ) - startTime
    print ( "==== Python Sequential NumPy NumExpr pi = " + str ( pi ) )
    print ( "==== Python Sequential NumPy NumExpr iteration count = " + str ( n ) )
    print ( "==== Python Sequential NumPy NumExpr elapse = " + str ( elapseTime ) )
    print ( "==== Python Sequential NumPy NumExpr thread count = " + str ( threadCount ) )
    print ( "==== Python Sequential NumPy NumExpr number of processors = " + str ( numexpr.detect_number_of_cores ( ) ) )
    
if __name__ == '__main__' :
    execute ( 1 )
    print
    execute ( 2 )
    print
    execute ( 8 )
    print
    execute ( 32 )
