/*
 *  A C program to calculate Pi using quadrature.  This is an SPMD realization using OpenMPI.
 *
 *  Copyright © 2008–2011 Russel Winder
 */

#include <stdio.h>
#include <mpi.h>
#include "microsecondTime.h"

int main ( int ac , char ** av ) { // MPI requires write access to these parameters!
  const int n = 1000000000 ;
  const double delta = 1.0 / n ;
  int nProcessors , myId ;
  const long long startTimeMicros = microsecondTime ( ) ;
  MPI_Init ( &ac , &av ) ;
  MPI_Comm_size ( MPI_COMM_WORLD , &nProcessors ) ;
  MPI_Comm_rank ( MPI_COMM_WORLD , &myId ) ;
  const int sliceSize = n / nProcessors ;
  const int start = 1 + myId * sliceSize ;
  const int end = ( myId + 1 ) * sliceSize ;
  double localSum = 0.0 ;
  for ( int i = start ; i <= end ; ++i ) {
    const double x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  double sum ;
  MPI_Reduce ( &localSum , &sum , 1 , MPI_DOUBLE , MPI_SUM , 0 , MPI_COMM_WORLD ) ;
  MPI_Finalize ( ) ;
  if ( myId == 0 ) {
    const double pi = 4.0 * delta * sum ;
    const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    printf ( "==== C MPI pi = %.18lf\n" , pi ) ;
    printf ( "==== C MPI iteration count = %d\n" , n ) ;
    printf ( "==== C MPI elapse time = %lf\n" , elapseTime ) ;
    printf ( "==== C MPI processor count = %d\n" ,  nProcessors ) ;
  }
  return 0 ;
}
