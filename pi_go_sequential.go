/*
 *  A Go program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2010–2012 Russel Winder
 */

package main

import (
	"fmt"
	"time"
)

func main ( ) {
	const n = 1000000000
	const delta = 1.0 / float64 ( n )
	startTime := time.Now ( )
	sum := float64 ( 0.0 )
	for  i := 1 ; i <= n ; i++ {
		x := ( float64 ( i ) - 0.5 ) * delta
		sum += 1.0 / ( 1.0 + x * x )
	}
	pi := 4.0 * delta * sum
	elapseTime := time.Now ( ).Sub ( startTime )
	fmt.Printf ( "==== Go Sequential pi = %.18f\n" , pi ) ;
	fmt.Printf ( "==== Go Sequential iteration count = %d\n" ,  n ) ;
	fmt.Printf ( "==== Go Sequential elapse = %v\n" , elapseTime ) ;
}
