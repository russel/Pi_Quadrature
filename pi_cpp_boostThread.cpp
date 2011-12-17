/*
 *  A C++ program to calculate Pi using quadrature as a threads-based algorithm.
 *
 *  Copyright © 2009–2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <boost/thread/thread.hpp>
#include "microsecondTime.h"

double sum ;
boost::mutex sumMutex ;

class PartialSum {
 private :
  int id ;
  int sliceSize ;
  double delta ;
 public :
  PartialSum ( const int i , const int s , const double d )
    : id ( i ) , sliceSize ( s ) , delta ( d ) { }
  void operator ( ) ( ) {
    const auto start = 1 + id * sliceSize ;
    const auto end = ( id + 1 ) * sliceSize ;
    auto localSum = 0.0 ;
    for ( auto i = start ; i <= end ; ++i ) {
      const auto x = ( i - 0.5 ) * delta ;
      localSum += 1.0 / ( 1.0 + x * x ) ;
    }
    boost::mutex::scoped_lock lock ( sumMutex ) ;
    sum += localSum ;
  }
};

void execute ( const int numberOfThreads ) {
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  const auto sliceSize = n / numberOfThreads ;
  boost::thread_group threads ;
  sum = 0.0 ;
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) { threads.create_thread ( PartialSum ( i , sliceSize , delta ) ) ; }
  threads.join_all ( ) ;
  const auto pi = 4.0 * delta * sum ;
  const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ Boost.Thread pi = " << std::setprecision ( 18 ) << pi << std::endl ;
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
