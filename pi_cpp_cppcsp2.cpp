#include <iostream>
#include <iomanip>
#include <vector>
#include <cppcsp/cppcsp.h>
#include "microsecondTime.h"

class Compute : public csp::CSProcess {
 private :
  const int id ;
  const long sliceSize ;
  const double delta ;
  const csp::Chanout<double> chanout ;
 public :
  Compute ( const int i , const long s , const double d , const csp::Chanout<double> c ) 
    : id ( i ) , sliceSize ( s ) , delta ( d ) , chanout ( c ) {
  }             
  void run ( ) {
    const long start = 1 + id * sliceSize ;
    const long end = ( id + 1 ) * sliceSize ;
    double sum = 0.0 ;
    for ( long i = start ; i <= end ; ++i ) {
      const double x = ( i - 0.5 ) * delta ;
      sum += 1.0 / ( 1.0 + x * x ) ;
    }
    chanout << sum ;
  }
} ;
  
class Accumulate : public csp::CSProcess {
 private :
  const long n ;
  const int numberOfProcesses ;
  const long long startTimeMicros ;
  const long sliceSize ;
  const double delta ;
  const csp::AltChanin<double> chanin ;
 public :
  Accumulate ( const long ni , const int np , const long long st , const long s , const double d , const csp::AltChanin<double> c ) 
    : n ( ni ) , numberOfProcesses ( np ) , startTimeMicros ( st ) , sliceSize ( s ) , delta ( d ) , chanin ( c ) {
  }             
  void run ( ) {
    double sum = 0.0 ;
    for ( int i = 0 ; i < numberOfProcesses ; ++i ) { double s ; chanin >> s ; sum += s ; }
    const double pi = 4.0 * sum * delta ;
    const double elapseTime = ( microsecondTime ( ) - startTimeMicros ) / 1e6 ;
    std::cout << "==== C++ C++CSP2 pi = " << std::setprecision ( 18 ) << pi << std::endl ;
    std::cout << "==== C++ C++CSP2 iteration count = " << n << std::endl ;
    std::cout << "==== C++ C++CSP2 elapse = " << elapseTime << std::endl ;
    std::cout << "==== C++ C++CSP2 process count = " <<  numberOfProcesses << std::endl ;
    //std::cout << "==== C++ C++CSP2 processor count = "  << std::thread::hardware_concurrency ( ) << std::endl ;
  }
} ;

void execute ( const int numberOfProcesses ) {
  const long n = 1000000000l ;
  const double delta = 1.0 / n ;
  const long long startTimeMicros = microsecondTime ( ) ;
  const long sliceSize = n / numberOfProcesses ;
  csp::Start_CPPCSP ( ) ;
  csp::Any2OneChannel<double> results ;
  std::vector<csp::CSProcessPtr> processes ;
  processes.push_back ( new Accumulate ( n , numberOfProcesses , startTimeMicros , sliceSize , delta , results.reader ( ) ) ) ;
  for ( int i = 0 ; i < numberOfProcesses ; ++i ) {
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
