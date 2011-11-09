/*
 *  A C++ program to calculate Pi using quadrature.  This uses Anthony Williams' Just::Threads Pro library which
 *  is an implementation of the threads specification of C++11 and has realizations of actors and dataflow.
 *
 *  Copyright © 2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <thread>

#include <jss/actor.hpp>

#include "microsecondTime.h"

void execute ( const int numberOfWorkerActors ) {
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  const auto sliceSize = n / numberOfWorkerActors ;
  // This actor cannot be const since if it is, the calculators cannot send to it.
  /*const*/ jss::actor accumulator (
                                //  Actors are not copyable and so cannot user the (=) form, must use the (&) form.
                                [ & ] ( ) {
                                  auto sum = 0.0 ;
                                  for ( auto i = 0 ; i < numberOfWorkerActors ; ++i ) {
                                    jss::actor::receive ( )
                                    .match<double> (
                                                    [ & ] ( double d ) {
                                                      sum += d ;
                                                    } ) ;
                                  }
                                  const auto pi = 4.0 * sum * delta ;
                                  const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
                                  std::cout << "==== C++ Just::Thread actors pi = " << std::setprecision ( 18 ) << pi << std::endl ;
                                  std::cout << "==== C++ Just::Thread actors iteration count = " << n << std::endl ;
                                  std::cout << "==== C++ Just::Thread actors elapse = " << elapseTime << std::endl ;
                                  std::cout << "==== C++ Just::Thread actors threadCount = " <<  numberOfWorkerActors << std::endl ;
                                  std::cout << "==== C++ Just::Thread actors processor count = "  << std::thread::hardware_concurrency ( ) << std::endl ;
                                } ) ;
  // jss:actor has no nullary constructor so cannot have arrays of jss::actor, must work with pointers.
  // Perhaps should use jss:actor_ref but this has no nullary constructor either.  The point here is to
  // ensure the actors have a life beyond the scope of creation -- otherwise they will simply be terminated
  // as the block terminates.  The other point is to spaqwn the actors rather than have them execute
  // sequentially.
  const jss::actor * calculators [ numberOfWorkerActors ] ;
  for ( auto index = 0 ; index < numberOfWorkerActors ; ++index ) {
    calculators [ index ] = new jss::actor a (
                                            // Actors are not copyable and so cannot user the (=) form, must use the (&) form.
                                            [ & ] ( ) {
                                              const auto start = 1 + index * sliceSize ;
                                              const auto end = ( index + 1 ) * sliceSize ;
                                              auto sum = 0.0 ;
                                              for ( auto i = start ; i < end ; ++i ) {
                                                const auto x = ( i - 0.5 ) * delta ;
                                                sum += 1.0 / ( 1.0 + x * x ) ;
                                              }
                                              accumulator.send ( sum ) ;
                                            } ) ;
  }
  for ( auto index = 0 ; index < numberOfWorkerActors ; ++index ) {
    calculators [ index ] -> ~actor ( ) ;
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
