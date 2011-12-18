#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2011 Russel Winder
 */

final n = 1000000 // 1000 times fewer due to speed issues.
final delta = 1.0 / n
final startTime = System.nanoTime ( )
final pi = 4 * delta * ( 1 .. n ).sum { i -> 1 / ( 1 + ( ( i - 0.5 ) * delta ) ** 2 ) }
final elapseTime =   ( System.nanoTime ( ) - startTime ) / 1e9
println ( "==== Groovy Sequential BigDecimal Sum Power pi = " + pi )
println ( "==== Groovy Sequential BigDecimal Sum Power iteration count = " + n ) 
println ( "==== Groovy Sequential BigDecimal Sum Power elapse = " + elapseTime )
