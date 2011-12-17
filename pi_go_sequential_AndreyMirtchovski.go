/*
 *  A Go program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2011 Russel Winder
 */

//  This variant of the sequential code was proposed by Andrey Mirtchovski in a private email in reply to an
//  email by Russel Winder on the GoLang Nuts email list.  It involves changing the types of n and i so as
//  to avoid the conversions from int to float64 in the inner loop.  This increases performance by 3 fold.
//  The speculation is that go 6g uses the CVTSL2SD instruction with an mmx register (the instruction
//  "converts doubleword integer to scalar double-precision floating-point value") and that the way the
//  toolchain generates code leads to code that stalls the pipeline.  But note this is an
//  hypothesis/speculation to explain the observed behaviour.  Note that the C and C++ code also uses this
//  instruction but fails to create the delay.

package main

import (
	"fmt"
	"time"
)

func main ( ) {
	const n = float64 ( 1000000000 )
	const delta = 1.0 / n
	startTime := time.Nanoseconds ( )
	sum := float64 ( 0.0 )
	for  i := float64 ( 1 ) ; i <= n ; i++ {
		x := ( i - 0.5 ) * delta
		sum += 1.0 / ( 1.0 + x * x )
	}
	pi := 4.0 * delta * sum
	elapseTime := float64 ( time.Nanoseconds ( ) - startTime ) / 1e9
	fmt.Printf ( "==== Go Sequential pi = %.18f\n" , pi ) ;
	fmt.Printf ( "==== Go Sequential iteration count = %f\n" ,  n ) ;
	fmt.Printf ( "==== Go Sequential elapse = %f\n" , elapseTime ) ;
}
