#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm and enforcing primitive
 *  types throughout.
 *
 *  Copyright © 2008–2011 Russel Winder
 */

// This is Groovy and so slow compared to Java.  Use primitive types though so as to avoid really bad
// performance due to use of Integer and BigDecimal.  Hence the careful markup of the literals as well as
// the variables.

//  This version is to show that JVM "warm up" doesn't really make much of a difference to Groovy
//  evaluation.  At least using ints.  Using longs there is a significant "warm up" for the JIT.

def execute ( ) { 
  final /*long */ int n = 100000000i // 10 times fewer due to speed issues.
  final double delta = 1.0d / n
  final startTime = System.nanoTime ( )
  double sum = 0.0d
  for ( /* Long */ int i in 1i .. n ) { sum += 1.0d / ( 1.0d + ( ( i - 0.5d ) * delta ) ** 2i ) }
  final double pi = 4.0d * delta * sum
  final elapseTime =   ( System.nanoTime ( ) - startTime ) / 1e9
  println ( "==== Groovy Sequential Primitives Warm Up pi = " + pi )
  println ( "==== Groovy Sequential Primitives Warm Up iteration count = " + n ) 
  println ( "==== Groovy Sequential Primitives Warm Up elapse = " + elapseTime )
}

execute ( )
println ( )
execute ( )
println ( )
execute ( )
println ( )
execute ( )
