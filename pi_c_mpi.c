/*
 *  A C program to calculate Pi using quadrature as an OpenMPI based algorithm.
 *
 *  Copyright © 2008-10 Russel Winder
 */

#include <stdio.h>
#include <mpi.h>
#include "microsecondTime.h"

int main ( int ac , char ** av ) { // MPI requires write access to ac and av :-(
  const long n = 1000000000l ;
  const long double delta = 1.0 / n ;
  int nProcessors , myId ;
  const long long startTimeMicros = microsecondTime ( ) ;
  MPI_Init ( &ac , &av ) ;
  MPI_Comm_size ( MPI_COMM_WORLD , &nProcessors ) ;
  MPI_Comm_rank ( MPI_COMM_WORLD , &myId ) ;
  const long sliceSize = n / nProcessors ;
  const long start = 1 + myId * sliceSize ;
  const long end = ( myId + 1 ) * sliceSize ;
  long double localSum = 0.0 ;
  long i ;
  for ( i = start ; i <= end ; ++i ) {
    const long double x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  long double sum ;
  MPI_Reduce ( &localSum , &sum , 1 , MPI_LONG_DOUBLE , MPI_SUM , 0 , MPI_COMM_WORLD ) ;
  MPI_Finalize ( ) ;
  if ( myId == 0 ) {
    const long double pi = 4.0 * sum * delta ;
    const long double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    printf ( "==== C MPI pi = %.25Lf\n" , pi ) ;
    printf ( "==== C MPI iteration count = %ld\n" , n ) ;
    printf ( "==== C MPI elapse time = %Lf\n" , elapseTime ) ;
    printf ( "==== C MPI processor count = %d\n" ,  nProcessors ) ;
  }
  return 0 ;
}
