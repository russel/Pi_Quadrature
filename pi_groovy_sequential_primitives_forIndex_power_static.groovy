#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

import groovy.transform.CompileStatic

@CompileStatic execute ( ) {
  final int n = 100000000i // 10 times fewer than Java due to speed issues.
  final double delta = 1.0d / n
  final startTimeNanos = System.nanoTime ( )
  double sum = 0.0d
  for ( int i = 1i ; i <= n ; ++i ) { sum += 1.0d / ( 1.0d + ( ( i - 0.5d ) * delta ) ** 2i ) }
  final double pi = 4.0d * delta * sum
  final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
  Output.out ( 'Groovy Sequential Primitive ForIndex Power Static' , pi , n , elapseTime )
}

execute ( )
