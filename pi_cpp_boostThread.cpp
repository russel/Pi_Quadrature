/*
 *  A C++ program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <boost/thread/thread.hpp>
#include "microsecondTime.h"

long double sum ;
boost::mutex sumMutex ;

class PartialSum {
 private :
  long start ;
  long end ;
  long double delta ;
 public :
  PartialSum ( const long s , const long e , const long double d )
    : start ( s ) , end ( e ) , delta ( d ) { }
  void operator ( ) ( ) {
    long double localSum = 0.0 ;
    for ( long i = start ; i <= end ; ++i ) {
      const long double x = ( i - 0.5 ) * delta ;
      localSum += 1.0 / ( 1.0 + x * x ) ;
    }
    boost::mutex::scoped_lock lock ( sumMutex ) ;
    sum += localSum ;
  }
};

void execute ( const int numberOfThreads ) {
  const long n = 1000000000l ;
  const long double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  const long sliceSize = n / numberOfThreads ;
  boost::thread_group threads ;
  sum = 0.0 ;
  for ( int i = 0 ; i < numberOfThreads ; ++i ) { threads.create_thread ( PartialSum ( 1 + i * sliceSize , ( i + 1 ) * sliceSize , delta ) ) ; }
  threads.join_all ( ) ;
  const long double pi = 4.0 * sum * delta ;
  const long double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ Boost.Thread pi = " << std::setprecision ( 25 ) << pi << std::endl ;
  std::cout << "==== C++ Boost.Thread iteration count = " << n << std::endl ;
  std::cout << "==== C++ Boost.Thread elapse = "   << elapseTime << std::endl ;
  std::cout << "==== C++ Boost.Thread thread count = " <<  numberOfThreads << std::endl ;
}

int main ( ) {
  execute ( 1 ) ;
  std::cout << std::endl ;
  execute ( 2 ) ;
  std::cout << std::endl ;
  execute ( 8 ) ;
  std::cout << std::endl ;
  execute ( 32 ) ;
  return 0 ;
}
