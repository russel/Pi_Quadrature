/*
 *  A C++ program to calculate Pi using quadrature.  This uses Anthony Williams' Just::Threads Pro library which
 *  is an implementation of the threads specification of C++11 and has realizations of actors and dataflow.
 *
 *  Copyright © 2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <memory>

#include <jss/actor.hpp>

#include "microsecondTime.h"

void execute ( const int numberOfWorkerActors ) {
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  const auto sliceSize = n / numberOfWorkerActors ;
  jss::actor accumulator (
                          //  jss::actors are not copyable and so cannot use the [=] capture form, must use
                          //  the [&] capture form.  Not a problem here as all the free variables in the
                          //  lambda expression are const variables.
                          [ & ] ( ) {
                            auto sum = 0.0 ;
                            for ( auto i = 0 ; i < numberOfWorkerActors ; ++i ) {
                              jss::actor::receive ( )
                               .match<double> (
                                               [ & ] ( double d ) {
                                                 sum += d ;
                                               } ) ;
                            }
                            const auto pi = 4.0 * delta * sum ;
                            const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
                            std::cout << "==== C++ Just::Thread actors pi = " << std::setprecision ( 18 ) << pi << std::endl ;
                            std::cout << "==== C++ Just::Thread actors iteration count = " << n << std::endl ;
                            std::cout << "==== C++ Just::Thread actors elapse = " << elapseTime << std::endl ;
                            std::cout << "==== C++ Just::Thread actors threadCount = " <<  numberOfWorkerActors << std::endl ;
                            std::cout << "==== C++ Just::Thread actors processor count = "  << std::thread::hardware_concurrency ( ) << std::endl ;
                          } ) ;
  std::unique_ptr<const jss::actor> calculators [ numberOfWorkerActors ] ;
  for ( auto index = 0 ; index < numberOfWorkerActors ; ++index ) {
    calculators [ index ] = std::unique_ptr<const jss::actor> ( new jss::actor (
                                                                                [ index , sliceSize , delta , &accumulator ] ( ) {
                                                                                  const auto start = 1 + index * sliceSize ;
                                                                                  const auto end = ( index + 1 ) * sliceSize ;
                                                                                  auto sum = 0.0 ;
                                                                                  for ( auto i = start ; i < end ; ++i ) {
                                                                                    const auto x = ( i - 0.5 ) * delta ;
                                                                                    sum += 1.0 / ( 1.0 + x * x ) ;
                                                                                  }
                                                                                  accumulator.send ( sum ) ;
                                                                                } ) ) ;
  }
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
