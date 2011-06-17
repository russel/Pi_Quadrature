/*
 *  A C++ program to calculate Pi using quadrature.  This uses Anthony Williams' Just::Threads library which
 *  is an implementation of the threads specification of C++0x.
 *
 *  This is a variant of pi_cpp_justThreadFutures.cpp from the Just::Thread tests -- Anthony took my
 *  examples and added them into the test suite but amended them a little.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <thread>
#include<future>
#include "microsecondTime.h"

double partialSum ( const int id , const int sliceSize , const double delta ) {
  const auto start = 1 + id * sliceSize ;
  const auto end = ( id + 1 ) * sliceSize ;
  auto sum = 0.0 ;
  for ( auto i = start ; i <= end ; ++i ) {
    const auto x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  return sum ;
}

void execute ( const int numberOfThreads ) {
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  const auto sliceSize = n / numberOfThreads ;
  std::packaged_task<double ( )> tasks [ numberOfThreads ] ;
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) {
    tasks[i] = std::packaged_task<double ( )> ( std::bind ( partialSum , i , sliceSize , delta ) ) ;
    std::thread taskThread ( std::ref ( tasks[i] ) ) ;
    taskThread.detach ( ) ;
  }
  auto sum = 0.0 ;
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) { sum += tasks[i].get_future ( ).get ( ) ; }
  const auto pi = 4.0 * sum * delta ;
  const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ Just::Thread futures AW pi = " << std::setprecision ( 18 ) << pi << std::endl ;
  std::cout << "==== C++ Just::Thread futures AW iteration count = " << n << std::endl ;
  std::cout << "==== C++ Just::Thread futures AW elapse = " << elapseTime << std::endl ;
  std::cout << "==== C++ Just::Thread futures AW threadCount = " <<  numberOfThreads << std::endl ;
  std::cout << "==== C++ Just::Thread futures AW processor count = "  << std::thread::hardware_concurrency ( ) << std::endl ;
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
