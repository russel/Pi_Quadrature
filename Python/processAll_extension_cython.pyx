#  A Cython extensions to calculate the main loop of π using quadrature.
#
#  Copyright © 2010–2014  Russel Winder

# cython: boundscheck=False

from cython.parallel import parallel, prange

def sequential(int n, double delta):
    cdef int i = 0
    cdef double sum = 0.0
    for i in range(1, n):
        x = (i - 0.5) * delta
        sum += 1.0 / (1.0 + x * x)
    return 4.0 * delta * sum

def parallel(int n, double delta):
    cdef int i = 0
    cdef double sum = 0.0
    cdef double x  # Type inference fails with prange :-(
    for i in prange(1, n, nogil=True):
        x = (i - 0.5) * delta
        sum += 1.0 / (1.0 + x * x)
    return 4.0 * delta * sum
