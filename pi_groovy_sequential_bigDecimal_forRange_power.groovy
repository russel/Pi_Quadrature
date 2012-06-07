#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

final n = 1000000 // 1000 times fewer than Java due to speed issues.
final delta = 1.0 / n
final startTime = System.nanoTime ( )
def sum = 0.0
for ( i in 1 .. n ) { sum += 1 / ( 1 + ( ( i - 0.5 ) * delta ) ** 2 ) }
final pi = 4 * delta * sum
final elapseTime =   ( System.nanoTime ( ) - startTime ) / 1e9
Output.out ( 'Groovy Sequential BigDecimal ForRange Power' , pi , n , elapseTime )
