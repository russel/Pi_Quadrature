/*
 *  A Go program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2010 Russel Winder
 */

package main

import (
	"fmt"
	"time"
)

func main ( ) {
	const n = int64 ( 1000000000 )
	const delta = 1.0 / float64 ( n )
	startTime := time.Nanoseconds ( )
	sum := float64 ( 0.0 )
	for  i := int64 ( 1 ) ; i <= n ; i++ {
		x := ( float64 ( i ) - 0.5 ) * delta
		sum += 1.0 / ( 1.0 + x * x )
	}
	pi := 4.0 * sum * delta
	elapseTime := float64 ( time.Nanoseconds ( ) - startTime ) / 1e9
	fmt.Printf ( "==== Go Sequential pi = %.18f\n" , pi ) ;
	fmt.Printf ( "==== Go Sequential iteration count = %d\n" ,  n ) ;
	fmt.Printf ( "==== Go Sequential elapse = %f\n" , elapseTime ) ;
}
