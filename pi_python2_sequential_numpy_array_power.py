#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm.  Use NumPy.
#
#  Copyright © 2008–2011 Russel Winder

import time
import numpy

n = 10000000 # 00
delta = 1.0 / n
startTime = time.time ( )
vectorFunction = numpy.vectorize ( lambda i : 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 ) )
pi = 4.0 * delta * vectorFunction ( numpy.arange ( n , dtype = numpy.float ) ).sum ( )
elapseTime = time.time ( ) - startTime
print ( "==== Python Sequential NumPy Array Power pi = " + str ( pi ) )
print ( "==== Python Sequential NumPy Array Power iteration count = " + str ( n ) )
print ( "==== Python Sequential NumPy Array Power elapse = " + str ( elapseTime ) )
