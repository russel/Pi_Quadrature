#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm.  For loop with xrange.  xrange only exists in
#  Python 2, in Python 3 range is what xrange used to be in Python 2.
#
#  Copyright © 2008–2011 Russel Winder

import time

n = 10000000 # 100 times fewer due to speed issues.
delta = 1.0 / n
startTime = time.time ( )
sum = 0.0
for i in xrange ( 1 , n + 1 ) :
  sum += 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 )
pi = 4.0 * delta * sum
elapseTime = time.time ( ) - startTime
print ( "==== Python Sequential For/Xrange Power pi = " + str ( pi ) )
print ( "==== Python Sequential For/Xrange Power iteration count = " + str ( n ) )
print ( "==== Python Sequential For/Xrange Power elapse = " + str ( elapseTime ) )
