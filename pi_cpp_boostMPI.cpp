/*
 *  A C++ program to calculate Pi using quadrature as anMPI-based algorithm.
 *
 *  Copyright © 2009–2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <boost/mpi.hpp>
#include "microsecondTime.h"

int main ( int ac , char * * av ) { // MPI requires writeable access to these parameters :-(
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  boost::mpi::environment environment ( ac , av ) ;
  boost::mpi::communicator world ;
  const auto nProcessors = world.size ( ) ;
  const auto myId = world.rank ( ) ;
  const auto sliceSize = n / nProcessors ;
  const auto start = 1 + myId * sliceSize ;
  const auto end = ( myId + 1 ) * sliceSize ;
  auto localSum = 0.0 ;
  for ( auto i = start ; i <= end ; ++i ) {
    const auto x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  auto sum = 0.0 ;
  boost::mpi::reduce ( world , localSum , sum , std::plus<double> ( ) , 0 ) ;
  if ( myId == 0 ) {
    const auto pi = 4.0 * delta * sum ;
    const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    std::cout << "==== C++ Boost MPI pi = " << std::setprecision ( 18 ) << pi << std::endl ;
    std::cout << "==== C++ Boost MPI iteration count = " << n << std::endl ;
    std::cout << "==== C++ Boost MPI elapse = " << elapseTime << std::endl ;
    std::cout << "==== C++ Boost MPI processor count = " << nProcessors << std::endl ; 
  }
  return 0 ;
}
