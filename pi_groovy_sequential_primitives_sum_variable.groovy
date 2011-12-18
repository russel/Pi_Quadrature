#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2011 Russel Winder
 */

// This is Groovy and so extraordinarily slow compared to Java.  Use primitive types though so as to avoid
// really bad performance due to use of Integer and BigDecimal.  Hence the careful markup of the literals as
// well as the variables.

final int n = 100000000i // 10 times fewer due to speed issues.
final double delta = 1.0d / n
final startTime = System.nanoTime ( )
final double pi = 4.0d * delta * ( 1i .. n ).sum { i ->
  final double x =  ( i - 0.5d ) * delta
  1.0d / ( 1.0d + x * x )
}
final elapseTime = ( System.nanoTime ( ) - startTime ) / 1e9
println ( "==== Groovy Sequential Primitives Sum Variable pi = " + pi )
println ( "==== Groovy Sequential Primitives Sum Variable iteration count = " + n ) 
println ( "==== Groovy Sequential Primitives Sum Variable elapse = " + elapseTime )
