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

//  Speed issue parameters: int is faster than long; classic for may be slower than "foreach" style for; **
//  operator appears slow in comparison to using a variable and * —
//  cf. pi_groovy_sequential_primitive_alt.groovy.

final int n = 100000000i // 10 times fewer due to speed issues.
final double delta = 1.0d / n
final startTime = System.nanoTime ( )
double sum = 0.0d
for ( int i in 1i .. n ) { sum += 1.0d / ( 1.0d + ( ( i - 0.5d ) * delta ) ** 2i ) }
final double pi = 4.0d * delta * sum
final elapseTime = ( System.nanoTime ( ) - startTime ) / 1e9
println ( "==== Groovy Sequential Primitives pi = " + pi )
println ( "==== Groovy Sequential Primitives iteration count = " + n ) 
println ( "==== Groovy Sequential Primitives elapse = " + elapseTime )
