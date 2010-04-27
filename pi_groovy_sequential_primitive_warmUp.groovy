#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008-10 Russel Winder
 */

// This is Groovy and so extraordinarily slow compared to Java.  Use primitive types though so as to avoid
// really bad performance due to use of Integer and BigDecimal.  Hence the careful markup of the literals as
// well as the variables.

//  Speed issue parameters: int is faster than long; classic for may be faster than "foreach" style for; **
//  operator appears slow in comparison to using a variable and * --
//  cf. pi_groovy_sequential_primitive_alt.groovy.

//  This version is to show that JVM warm up doesn't really make much of a difference to Groovy evaluation.

def execute ( ) { 
  final int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final long startTime = System.nanoTime ( )
  double sum = 0.0d
  for ( int i in 1..n ) { sum += 1.0d / ( 1.0d + ( ( i - 0.5d ) * delta ) ** 2i ) }
  final double pi = 4.0d * sum * delta
  final double elapseTime =   ( System.nanoTime ( ) - startTime ) / 1e9
  println ( "==== Groovy Sequential Primitives pi = " + pi )
  println ( "==== Groovy Sequential Primitives iteration count = " + n ) 
  println ( "==== Groovy Sequential Primitives elapse = " + elapseTime )
}

execute ( )
println ( )
execute ( )
println ( )
execute ( )
println ( )
execute ( )
