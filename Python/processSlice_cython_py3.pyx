#  A Cython extensions to calculate a slice of the overall calculation of π using quadrature.
#
#  Copyright © 2010–2013 Russel Winder

def processSlice(int id, int sliceSize, double delta):
  cdef int i = 0
  cdef double sum = 0.0
  for i in range(1 + id * sliceSize, (id + 1) * sliceSize):
    x = (i - 0.5) * delta
    sum += 1.0 / (1.0 + x * x)
  return sum
