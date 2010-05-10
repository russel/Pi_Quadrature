# -*- mode:python; coding:utf-8; -*-

#  A Cython extensions to calculate a slice of the overall calculation of Pi using quadrature.
#
#  Copyright © 2010 Russel Winder

def processSlice ( long id , long sliceSize , double delta ) :
  cdef long i = 0
  cdef double sum = 0.0
  cdef double x
  for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize ) :
    x = ( i - 0.5 ) * delta
    sum += 1.0 / ( 1.0 + x * x )
  return sum
