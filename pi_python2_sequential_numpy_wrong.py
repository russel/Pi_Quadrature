#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm.  Use NumPy.
#
#  Copyright © 2008–2011 Russel Winder

import time
import numpy

n = numpy.int ( 1000000 ) # 000 )
delta = numpy.divide ( 1.0 , n )
startTime = time.time ( )
sum = numpy.float ( 0.0 )
for i in xrange ( 1 , n + 1 ) :
  x = numpy.multiply ( numpy.subtract ( i , 0.5 ) , delta )
  sum = numpy.add ( sum , numpy.divide ( 1.0 , numpy.add ( 1.0 , numpy.multiply ( x , x ) ) ) )
pi = numpy.multiply ( 4.0 , numpy.multiply ( delta , sum ) )
elapseTime = time.time ( ) - startTime
print ( "==== Python Sequential NumPy Wrong pi = " + str ( pi ) )
print ( "==== Python Sequential NumPy Wrong iteration count = " + str ( n ) )
print ( "==== Python Sequential NumPy Wrong elapse = " + str ( elapseTime ) )
