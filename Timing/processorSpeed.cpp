/*
 *  A C++ function to return the speed of the processor in GHz.
 *
 *  Copyright © 2008 Russel Winder
 */

#include <string>
#include <fstream>
#include <cstdlib>

#include "processorSpeed.hpp"

double processorSpeed ( ) {

#if defined ( __linux__ )

  std::ifstream cpuinfo ( "/proc/cpuinfo" ) ;
  while ( cpuinfo.good ( ) ) {
    std::string buffer ;
    getline ( cpuinfo , buffer ) ;
    std::size_t index = buffer.find ( ':' ) ;
    std::string tag = buffer.substr ( 0 , index ) ;
    tag = tag.substr ( 0 , tag.find_last_not_of ( "\t " ) + 1 ) ;
    std::string value = buffer.substr ( index + 2 ) ;
    if ( tag == "cpu MHz" ) {
      return std::atof ( value.c_str ( ) ) / 1000 ;
    }
  }
  return 0.0 ;

#endif /* ( defined ( __linux__ ) */

}
