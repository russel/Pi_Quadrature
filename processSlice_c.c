/*
 *  A C function to calculate a slice of the overall calculation of Pi using quadrature.
 *
 *  Copyright © 2009-10 Russel Winder
 */

double processSlice ( long id , long sliceSize , double delta ) {
  const long start = 1 + id * sliceSize ;
  const long end = ( id + 1 ) * sliceSize ;
  double sum = 0.0 ;
  long i ;
  for ( i = start ; i <= end ; ++i ) {
    const double x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  return sum ;
}
