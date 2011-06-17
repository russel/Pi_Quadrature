/*
 *  A C++ program to calculate Pi using quadrature.  This uses Anthony Williams' Just::Threads library which
 *  is an implementation of the threads specification of C++0x.
 *
 *  Copyright © 2009--2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <thread>
#include<mutex>
#include "microsecondTime.h"

double sum ;
std::mutex sumMutex ;

void partialSum ( const int id , const int sliceSize , const double delta ) {
  const auto start = 1 + id * sliceSize ;
  const auto end = ( id + 1 ) * sliceSize ;
  auto localSum = 0.0 ;
  for ( auto i = start ; i <= end ; ++i ) {
    const auto x = ( i - 0.5 ) * delta ;
    localSum += 1.0 / ( 1.0 + x * x ) ;
  }
  std::lock_guard<std::mutex> lock ( sumMutex ) ;
  sum += localSum ;
}

void execute ( const int numberOfThreads ) {
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  const auto sliceSize = n / numberOfThreads ;
  std::thread threads [ numberOfThreads ] ;
  sum = 0.0 ;
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) { threads[i] = std::thread ( std::bind ( partialSum , i , sliceSize , delta ) ) ; }
  for ( auto i = 0 ; i < numberOfThreads ; ++i ) { threads[i].join ( ) ; }
  const auto pi = 4.0 * sum * delta ;
  const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
  std::cout << "==== C++ Just::Thread threads pi = " << std::setprecision ( 18 ) << pi << std::endl ;
  std::cout << "==== C++ Just::Thread threads iteration count = " << n << std::endl ;
  std::cout << "==== C++ Just::Thread threads elapse = " << elapseTime << std::endl ;
  std::cout << "==== C++ Just::Thread threads threadCount = " <<  numberOfThreads << std::endl ;
  std::cout << "==== C++ Just::Thread threads processor count = "  << std::thread::hardware_concurrency ( ) << std::endl ;
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
