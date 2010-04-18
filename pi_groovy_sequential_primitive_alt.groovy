#! /usr/bin/env groovy 

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008-10 Russel Winder
 */

// This is Groovy and so extraordinarily slow compared to Java.  Use primitive types though so as to avoid
// really bad performance due to use of Integer and BigDecimal.  Hence the careful markup of the literals as
// well as the variables.

//  Speed issue parameters: int is faster than long; classic for may be faster than "foreach" style for; **
//  operator appears slow in comparison to using a variable and * --
//  cf. pi_groovy_sequential_primitive.groovy.

final int n = 100000000i // 10 times fewer due to speed issues.
final double delta = 1.0d / n
final long startTimeNanos = System.nanoTime ( )
double sum = 0.0d
for ( int i = 1i ; i <= n ; ++i ) {
  final double x = ( i - 0.5d ) * delta
  sum += 1.0d / ( 1.0d + x * x )
}
final double pi = 4.0d * sum * delta
final double elapseTime = ( System.nanoTime ( ) - startTimeNanos ) / 1e9
System.out.println ( "==== Groovy Sequential Primitive Alt pi = " + pi )
System.out.println ( "==== Groovy Sequential Primitive Alt iteration count = " + n )
System.out.println ( "==== Groovy Sequential Primitive Alt elapse = " + elapseTime )
