#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

import groovy.transform.CompileStatic

@CompileStatic execute ( ) {
  final int n = 1000000000i
  final double delta = 1.0d / n
  final startTime = System.nanoTime ( )
  double sum = 0.0d
  for ( int i in 1i .. n ) { sum += 1.0d / ( 1.0d + ( ( i - 0.5d ) * delta ) ** 2i ) }
  final double pi = 4.0d * delta * sum
  final elapseTime = ( System.nanoTime ( ) - startTime ) / 1e9
  Output.out ( 'Groovy Sequential Primitives ForRange Power Static' , pi , n , elapseTime )
}

execute ( )