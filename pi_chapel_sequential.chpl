/*
 *  A Chapel program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2009 Russel Winder
 */

use Time ;

param n : int(64) = 1000000000 ;
const delta : real = 1.0 / n ;
var timer : Timer ;
timer.start ( ) ;
var sum : real = 0.0 ;
var i : int(64) = 0 ;
while ( i < n ) {
  sum += 1.0 / ( 1.0 + ( ( i - 0.5 ) * delta ) ** 2 ) ;
  i += 1 ;
 }
const pi = 4.0 * sum * delta ;
timer.stop ( ) ;
writeln ( "==== Chapel Sequential pi = " , pi ) ;
writeln ( "==== Chapel Sequential iteration count = " , n ) ;
writeln ( "==== Chapel Sequential elapse = " , timer.elapsed ( ) ) ;
