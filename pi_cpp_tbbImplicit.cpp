/*
 *  A C program to calculate Pi using quadrature as a TBB implemented algorithm.
 *
 *  Copyright © 2009 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include "tbb/task_scheduler_init.h"
#include "tbb/blocked_range.h"
#include "tbb/parallel_reduce.h"
#include "microsecondTime.h"

class partialSum {
 private :
  long double delta ;
  long double sum ;
 public :
  partialSum ( const long double d ) : delta ( d ) , sum ( 0.0 ) { }
  partialSum ( const partialSum & x , tbb::split ) : delta ( x.delta ) , sum ( 0.0 ) { }
  void operator ( ) ( const tbb::blocked_range<long>& range ) {
    for ( long i = range.begin ( ) ; i != range.end ( ) ; ++i ) {
      const long double x = ( i - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
  }
  void join ( const partialSum & x ) { sum += x.sum ; }
  long double getSum ( ) { return sum ; }
} ;

int main ( ) {
  const long n = 1000000000l ;
  const long double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  tbb::task_scheduler_init tbb_initializer ;
  partialSum accumulator ( delta ) ;
  tbb::parallel_reduce ( tbb::blocked_range<long> ( 0 , n ) , accumulator , tbb::auto_partitioner ( ) ) ;
  const long double pi = 4.0 * accumulator.getSum ( ) * delta ;
  const long double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ TBB pi = " << std::setprecision ( 25 ) << pi << std::endl ;
  std::cout << "==== C++ TBB iteration count = " << n << std::endl ;
  std::cout << "==== C++ TBB elapse = "  << elapseTime << std::endl ;
  return 0 ;
}
