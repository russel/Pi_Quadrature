#! /usr/bin/env groovy

/*
 *  Calculation of Pi using quadrature realized with a basic sequential algorithm.
 *
 *  Copyright © 2008–2012 Russel Winder
 */

final n = 1000000 // 1000 times fewer than Java due to speed issues.
final delta = 1.0 / n
final startTime = System.nanoTime ( )
final pi = 4 * delta * ( 1 .. n ).sum { i -> 1 / ( 1 + ( ( i - 0.5 ) * delta ) ** 2 ) }
final elapseTime =   ( System.nanoTime ( ) - startTime ) / 1e9
Output.out ( getClass ( ).name , pi , n , elapseTime )
