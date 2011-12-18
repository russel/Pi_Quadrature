#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2011 Russel Winder
 */

// This is Groovy and so extraordinarily slow compared to Java.  Use primitive types though so as to avoid
// really bad performance due to use of Integer and BigDecimal.  Hence the careful markup of the literals as
// well as the variables.

//  Speed issue parameters: int is faster than long; classic for may be slower than "foreach" style for; **
//  operator appears slow in comparison to using a variable and * —
//  cf. pi_groovy_sequential_primitive.groovy.

final int n = 100000000i // 10 times fewer due to speed issues.
final double delta = 1.0d / n
final startTimeNanos = System.nanoTime ( )
double sum = 0.0d
for ( int i = 1i ; i <= n ; ++i ) { sum += 1.0d / ( 1.0d + ( ( i - 0.5d ) * delta ) ** 2i ) }
final double pi = 4.0d * delta * sum
final elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
println ( "==== Groovy Sequential Primitive For Power pi = " + pi )
println ( "==== Groovy Sequential Primitive For Power iteration count = " + n )
println ( "==== Groovy Sequential Primitive For Power elapse = " + elapseTime )
