/*
 *  A C++ function to calculate a slice of the overall calculation of Pi using quadrature.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

extern "C" double processSlice ( int id , int sliceSize , double delta ) ;

double processSlice ( const int id , const int sliceSize , const double delta ) {
  const auto start = 1 + id * sliceSize ;
  const auto end = ( id + 1 ) * sliceSize ;
  auto sum = 0.0 ;
  for ( auto i = start ; i <= end ; ++i ) {
    const auto x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  return sum ;
}
