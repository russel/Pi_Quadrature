/*
 *  A C++ program to calculate Pi using quadrature as a CSP implemented algorithm.
 *
 *  Copyright © 2010--2011 Russel Winder
 */

#include <iostream>
#include <iomanip>
#include <vector>
#include <cppcsp/cppcsp.h>
#include "microsecondTime.h"

class Compute : public csp::CSProcess {
 private :
  const int id ;
  const int sliceSize ;
  const double delta ;
  const csp::Chanout<double> chanout ;
 public :
  Compute ( const int i , const int s , const double d , const csp::Chanout<double> c ) 
    : id ( i ) , sliceSize ( s ) , delta ( d ) , chanout ( c ) {
  }             
  void run ( ) {
    const auto start = 1 + id * sliceSize ;
    const auto end = ( id + 1 ) * sliceSize ;
    auto sum = 0.0 ;
    for ( auto i = start ; i <= end ; ++i ) {
      const auto x = ( i - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
    chanout << sum ;
  }
} ;
  
class Accumulate : public csp::CSProcess {
 private :
  const int n ;
  const int numberOfProcesses ;
  const long long startTimeMicros ;
  const int sliceSize ;
  const double delta ;
  const csp::AltChanin<double> chanin ;
 public :
  Accumulate ( const int ni , const int np , const long long st , const int s , const double d , const csp::AltChanin<double> c ) 
    : n ( ni ) , numberOfProcesses ( np ) , startTimeMicros ( st ) , sliceSize ( s ) , delta ( d ) , chanin ( c ) {
  }             
  void run ( ) {
    auto sum = 0.0 ;
    for ( auto i = 0 ; i < numberOfProcesses ; ++i ) { double s ; chanin >> s ; sum += s ; }
    const auto pi = 4.0 * sum * delta ;
    const auto elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    std::cout << "==== C++ C++CSP2 pi = " << std::setprecision ( 18 ) << pi << std::endl ;
    std::cout << "==== C++ C++CSP2 iteration count = " << n << std::endl ;
    std::cout << "==== C++ C++CSP2 elapse = " << elapseTime << std::endl ;
    std::cout << "==== C++ C++CSP2 process count = " <<  numberOfProcesses << std::endl ;
    //std::cout << "==== C++ C++CSP2 processor count = "  << std::thread::hardware_concurrency ( ) << std::endl ;
  }
} ;

void execute ( const int numberOfProcesses ) {
  const auto n = 1000000000 ;
  const auto delta = 1.0 / n ;
  const auto startTimeMicros = microsecondTime ( ) ;
  const auto sliceSize = n / numberOfProcesses ;
  csp::Start_CPPCSP ( ) ;
  csp::Any2OneChannel<double> results ;
  std::vector<csp::CSProcessPtr> processes ;
  processes.push_back ( new Accumulate ( n , numberOfProcesses , startTimeMicros , sliceSize , delta , results.reader ( ) ) ) ;
  for ( auto i = 0 ; i < numberOfProcesses ; ++i ) {
    processes.push_back ( new Compute ( i , sliceSize , delta , results.writer ( ) ) ) ;
  }
  csp::Run ( csp::InParallel ( processes.begin ( ) , processes.end ( ) ) ) ;
  csp::End_CPPCSP ( ) ;
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
