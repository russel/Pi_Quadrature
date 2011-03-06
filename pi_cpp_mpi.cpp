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
  const int n = 1000000000 ;
  const double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  MPI::Init ( ac , av ) ;
  const int nProcessors = MPI::COMM_WORLD.Get_size ( ) ;
  const int myId = MPI::COMM_WORLD.Get_rank ( ) ;
  //std::cout << "Node of rank " << myId << " working." << std::endl ;
  const int sliceSize = n / nProcessors ;
  const int start = 1 + myId * sliceSize ;
  const int end = ( myId + 1 ) * sliceSize ;
  double localSum = 0.0 ;
  for ( int i = start ; i <= end ; ++i ) {
    const double x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  double sum ;
  MPI::COMM_WORLD.Reduce ( &localSum , &sum , 1 , MPI::DOUBLE , MPI::SUM , 0 ) ;
  MPI::Finalize ( ) ;
  if ( myId == 0 ) {
    const double pi = 4.0 * sum * delta ;
    const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    std::cout << "==== C++ MPI pi = " << std::setprecision ( 18 ) << pi << std::endl ;
    std::cout << "==== C++ MPI iteration count = " << n << std::endl ;
    std::cout << "==== C++ MPI elapse = " << elapseTime << std::endl ;
    std::cout << "==== C++ MPI processor count = " << nProcessors << std::endl ; 
  }
  return 0 ;
}
