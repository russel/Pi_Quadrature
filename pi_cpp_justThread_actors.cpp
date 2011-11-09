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
  //  This actor cannot be const since if it is, the calculators cannot send to it.  Not unreasonable since
  //  sending a message to an actor is a state change of that actor after all.
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
                                                 std::cout << "Received " << d << ", sum = " << sum << std::endl ;
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
  //  We have to use dynamic allocation for the worker actors since stack allocated variables have lifetime
  //  the same as scope so this would sequentialize the execution of the actors: the jss::actor destructor
  //  waits for the end of the bound thread.
  std::unique_ptr<const jss::actor> calculators [ numberOfWorkerActors ] ;
  for ( auto index = 0 ; index < numberOfWorkerActors ; ++index ) {
    //  Since actors are not copyable it is not possible to use the [=] capture form for the lambda
    //  function.  Instead the [&] form must be used.  If only const variables need to be captured this is
    //  not a problem.  However, in this case, non-const variables need to be captured and the captured
    //  value must be the value at the time of capture and not the value at the end of the loop.  Since the
    //  [&] form captures the address of a variable not it's value, we must use the old Java trick of
    //  assigning to a const variable in order to capture the value of the iteration variable rather than
    //  it's address.  Hack, hack.
    //
    //  Currently we have a problem that we get multiple copies of some slices and are missing some slices
    //  this leads to the wrong value being calculated.
    const auto constIndex = index ;
    calculators [ index ] = std::unique_ptr<const jss::actor> ( new jss::actor (
                                                                                [ & ] ( ) {
                                                                                  const auto start = 1 + constIndex * sliceSize ;
                                                                                  const auto end = ( constIndex + 1 ) * sliceSize ;
                                                                                  auto sum = 0.0 ;
                                                                                  for ( auto i = start ; i < end ; ++i ) {
                                                                                    const auto x = ( i - 0.5 ) * delta ;
                                                                                    sum += 1.0 / ( 1.0 + x * x ) ;
                                                                                  }
                                                                                  std::cout << "Actor " << constIndex << ", sending " << sum << std::endl ;
                                                                                  accumulator.send ( sum ) ;
                                                                                } ) ) ;
  }
  // Due to the accumulator constructor being called at this point, we should wait for termination of said
  // actor before the program finishes.  The outstanding question is whether all the sources have sent in
  // their results.  Assume they have and have terminated.
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
