#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

final int n = 100000000i // 10 times fewer than Java due to speed issues.
final double delta = 1.0d / n
final startTime = System.nanoTime ( )
double sum = 0.0d
for ( int i in 1i .. n ) {
  final double x =  ( i - 0.5d ) * delta
  sum += 1.0d / ( 1.0d + x * x )
}
final double pi = 4.0d * delta * sum
final elapseTime = ( System.nanoTime ( ) - startTime ) / 1e9
Output.out ( 'Groovy Sequential Primitives ForRange Multiply Dynamic' , pi , n , elapseTime )
