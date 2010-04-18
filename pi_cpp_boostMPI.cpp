/*
 *  A C++ program to calculate Pi using quadrature as anMPI-based algorithm.
 *
 *  Copyright © 2009 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <boost/mpi.hpp>
#include "microsecondTime.h"

int main ( int ac , char * * av ) { // MPI requires writeable access to these parameters :-(
  const long n = 1000000000l ;
  const long double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  boost::mpi::environment environment ( ac , av ) ;
  boost::mpi::communicator world ;
  const int nProcessors = world.size ( ) ;
  const int myId = world.rank ( ) ;
  const long sliceSize = n / nProcessors ;
  const long start = 1 + myId * sliceSize ;
  const long end = ( myId + 1 ) * sliceSize ;
  long double localSum = 0.0 ;
  for ( long i = start ; i <= end ; ++i ) {
    const long double x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  long double sum ;
  boost::mpi::reduce ( world , localSum , sum , std::plus<long double> ( ) , 0 ) ;
  if ( myId == 0 ) {
    const long double pi = 4.0 * sum * delta ;
    const long double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    std::cout << "==== C++ Boost MPI pi = " << std::setprecision ( 25 ) << pi << std::endl ;
    std::cout << "==== C++ Boost MPI iteration count = " << n << std::endl ;
    std::cout << "==== C++ Boost MPI elapse = " << elapseTime << std::endl ;
    std::cout << "==== C++ Boost MPI processor count = " << nProcessors << std::endl ; 
  }
  return 0 ;
}
