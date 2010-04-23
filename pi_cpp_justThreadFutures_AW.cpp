/*
 *  A C++ program to calculate Pi using quadrature.  This uses Anthony Williams' Just::Threads library which
 *  is an implementation of the threads specification of C++0x.
 *
 *  This is a variant of pi_cpp_justThreadFutures.cpp from the Just::Thread tests -- Anthony took my
 *  examples and added them into the test suite but amended them a little.
 *
 *  Copyright © 2009-10 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <thread>
#include<future>
#include "microsecondTime.h"

long double partialSum ( const long id , const long sliceSize , const long double delta ) {
  const long start = 1 + id * sliceSize ;
  const long end = ( id + 1 ) * sliceSize ;
  long double sum = 0.0 ;
  for ( long i = start ; i <= end ; ++i ) {
    const long double x = ( i - 0.5 ) * delta ;
    sum += 1.0 / ( 1.0 + x * x ) ;
  }
  return sum ;
}

void execute ( const int numberOfThreads ) {
  const long n = 1000000000l ;
  const long double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  const long sliceSize = n / numberOfThreads ;
  std::packaged_task<long double ( )> tasks [ numberOfThreads ] ;
  for ( int i = 0 ; i < numberOfThreads ; ++i ) {
    tasks[i] = std::packaged_task<long double ( )> ( std::bind ( partialSum , i , sliceSize , delta ) ) ;
    std::thread taskThread ( std::ref ( tasks[i] ) ) ;
    taskThread.detach ( ) ;
  }
  long double sum = 0.0 ;
  for ( int i = 0 ; i < numberOfThreads ; ++i ) { sum += tasks[i].get_future ( ).get ( ) ; }
  const long double pi = 4.0 * sum * delta ;
  const long double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ Just::Thread futures AW pi = " << std::setprecision ( 25 ) << pi << std::endl ;
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
