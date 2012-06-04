#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

final n = 1000000 // 1000 times fewer than Java due to speed issues.
final delta = 1.0 / n
final startTimeNanos = System.nanoTime ( )
def sum = 0.0
for ( i = 1 ; i <= n ; ++i ) {
  final x = ( i - 0.5 ) * delta
  sum += 1.0 / ( 1.0 + x * x )
}
final double pi = 4.0 * delta * sum
final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
println ( "==== Groovy Sequential Primitive ForIndex Multiply pi = " + pi )
println ( "==== Groovy Sequential Primitive ForIndex Multiply iteration count = " + n )
println ( "==== Groovy Sequential Primitive ForIndex Multiply elapse = " + elapseTime )
