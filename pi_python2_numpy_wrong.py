#! /usr/bin/env python
# -*- mode:python; coding:utf-8; -*-

#  Calculation of Pi using quadrature. Sequential algorithm.  Use NumPy.
#
#  Copyright © 2008–2012 Russel Winder

from numpy import int , float , add , subtract , multiply , divide 
from output import out
from time import time

if __name__ == '__main__' :
  n = int ( 1000000 ) # 1000 times fewer than C due to speed reasons.
  delta = divide ( 1.0 , n )
  startTime = time ( )
  sum = float ( 0.0 )
  for i in xrange ( 1 , n + 1 ) :
    x = multiply ( subtract ( i , 0.5 ) , delta )
    sum = add ( sum , divide ( 1.0 , add ( 1.0 , multiply ( x , x ) ) ) )
  pi = multiply ( 4.0 , multiply ( delta , sum ) )
  elapseTime = time ( ) - startTime
  out ( __file__ , pi , n , elapseTime )
