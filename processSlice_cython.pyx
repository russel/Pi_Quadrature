def processSlice ( long id , long sliceSize , double delta ) :
  cdef long i = 0
  cdef long double sum = 0.0
  cdef long double x
  for i in xrange ( 1 + id * sliceSize , ( id + 1 ) * sliceSize ) :
    x = ( i - 0.5 ) * delta
    sum += 1.0 / ( 1.0 + x * x )
  return sum
