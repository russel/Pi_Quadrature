/*
 *  A C++ program to calculate Pi using quadrature.  This is an SPMD realization using OpenMPI.
 *
 *  Copyright © 2008--2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <mpi.h>
#include "microsecondTime.h"

int main ( int ac , char * * av ) { // MPI requires writeable access to these parameters!
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  MPI::Init ( ac , av ) ;
  const auto nProcessors = MPI::COMM_WORLD.Get_size ( ) ;
  const auto myId = MPI::COMM_WORLD.Get_rank ( ) ;
  //std::cout << "Node of rank " << myId << " working." << std::endl ;
  const auto sliceSize = n / nProcessors ;
  const auto start = 1 + myId * sliceSize ;
  const auto end = ( myId + 1 ) * sliceSize ;
  auto localSum = 0.0 ;
  for ( auto i = start ; i <= end ; ++i ) {
    const auto x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  auto sum = 0.0 ;
  MPI::COMM_WORLD.Reduce ( &localSum , &sum , 1 , MPI::DOUBLE , MPI::SUM , 0 ) ;
  MPI::Finalize ( ) ;
  if ( myId == 0 ) {
    const auto pi = 4.0 * sum * delta ;
    const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    std::cout << "==== C++ MPI pi = " << std::setprecision ( 18 ) << pi << std::endl ;
    std::cout << "==== C++ MPI iteration count = " << n << std::endl ;
    std::cout << "==== C++ MPI elapse = " << elapseTime << std::endl ;
    std::cout << "==== C++ MPI processor count = " << nProcessors << std::endl ; 
  }
  return 0 ;
}
