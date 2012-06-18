#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm. Use NumPy.
#
#  Copyright © 2008–2012 Russel Winder

from numpy import arange , float , frompyfunc
from output import out
from time import time

if __name__ == '__main__' :
    n = 10000000 # 00
    delta = 1.0 / n
    startTime = time ( )
    pi = 4.0 * delta * frompyfunc ( lambda i : 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 ) , 1 , 1 ) ( arange ( n , dtype = float ) ).sum ( )
    elapseTime = time ( ) - startTime
    out ( __file__ , pi , n , elapseTime )
