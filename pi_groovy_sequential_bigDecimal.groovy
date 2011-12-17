#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2011 Russel Winder
 */

def n = 10000000 // 100 times fewer due to speed issues.
def delta = 1.0 / n
def startTime = System.nanoTime ( )
def sum = 0.0
for ( i in 1 .. n ) { sum += 1 / ( 1 + ( ( i - 0.5 ) * delta ) ** 2 ) }
def pi = 4 * delta * sum
def elapseTime =   ( System.nanoTime ( ) - startTime ) / 1e9
println ( "==== Groovy Sequential BigDecimal pi = " + pi )
println ( "==== Groovy Sequential BigDecimal iteration count = " + n ) 
println ( "==== Groovy Sequential BigDecimal elapse = " + elapseTime )
