#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm. Use reduce.
#
#  Copyright © 2008–2012 Russel Winder

from output import out
from time import time

if __name__ == '__main__' :
    n = 10000000 # 100 times fewer than C due to speed issues.
    delta = 1.0 / n
    startTime = time ( )
    pi = 4.0 * delta * reduce ( lambda s , i :  s + 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 ) , xrange ( n ) , 0.0 )
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime )
