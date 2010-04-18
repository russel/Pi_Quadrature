/*
 *  A C++ program to calculate Pi using quadrature as an MPI based algorithm.
 *
 *  Copyright © 2008-9 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <mpi.h>
#include "microsecondTime.h"

int main ( int ac , char * * av ) { // MPI requires writeable access to these parameters :-(
  const long n = 1000000000l ;
  const long double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  MPI::Init ( ac , av ) ;
  const int nProcessors = MPI::COMM_WORLD.Get_size ( ) ;
  const int myId = MPI::COMM_WORLD.Get_rank ( ) ;
  const long sliceSize = n / nProcessors ;
  const long start = 1 + myId * sliceSize ;
  const long end = ( myId + 1 ) * sliceSize ;
  long double localSum = 0.0 ;
  for ( long i = start ; i <= end ; ++i ) {
    const long double x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  long double sum ;
  MPI::COMM_WORLD.Reduce ( &localSum , &sum , 1 , MPI::LONG_DOUBLE , MPI::SUM , 0 ) ;
  MPI::Finalize ( ) ;
  if ( myId == 0 ) {
    const long double pi = 4.0 * sum * delta ;
    const long double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    std::cout << "==== C++ MPI pi = " << std::setprecision ( 25 ) << pi << std::endl ;
    std::cout << "==== C++ MPI iteration count = " << n << std::endl ;
    std::cout << "==== C++ MPI elapse = " << elapseTime << std::endl ;
    std::cout << "==== C++ MPI processor count = " << nProcessors << std::endl ; 
  }
  return 0 ;
}
