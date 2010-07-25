# -*- mode:python; coding:utf-8; -*-

#  A Pyrex extensions to calculate a slice of the overall calculation of Pi using quadrature.
#
#  Copyright © 2010 Russel Winder

def processSlice ( long id , long sliceSize , double delta ) :
  cdef long i
  cdef double sum , x
  sum = 0.0
  for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize + 1 ) :
    x = ( i - 0.5 ) * delta
    sum += 1.0 / ( 1.0 + x * x )
  return sum
