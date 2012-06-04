#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

final int n = 100000000i // 10 times fewer than Java due to speed issues.
final double delta = 1.0d / n
final startTimeNanos = System.nanoTime ( )
double sum = 0.0d
for ( int i = 1i ; i <= n ; ++i ) {
  final double x = ( i - 0.5d ) * delta
  sum += 1.0d / ( 1.0d + x * x )
}
final double pi = 4.0d * delta * sum
final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
println ( "==== Groovy Sequential Primitive For Multiply pi = " + pi )
println ( "==== Groovy Sequential Primitive For Multiply iteration count = " + n )
println ( "==== Groovy Sequential Primitive For Multiply elapse = " + elapseTime )
