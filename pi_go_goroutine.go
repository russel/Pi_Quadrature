/*
 *  A Go program to calculate Pi using quadrature as a sequential algorithm.
 *
 *  Copyright © 2010 Russel Winder
 */

package main

import (
	"fmt"
	"time"
	"runtime"
)

func processSlice ( id int , sliceSize int , delta float64 , channel chan float64 ) {
	start := 1 + id * sliceSize
	end := ( id + 1 ) * sliceSize
	sum := float64 ( 0.0 )
	for i := start ; i <= end ; i++ {
		x := ( float64 ( i ) - 0.5 ) * delta
		sum += 1.0 / ( 1.0 + x * x )
	}
	channel <- sum
}

func execute ( numberOfTasks int ) {
	const n = 1000000000
	const delta = 1.0 / float64 ( n )
	startTime := time.Nanoseconds ( )
	runtime.GOMAXPROCS ( numberOfTasks ) // Temporary hack
	sliceSize := n / numberOfTasks
	channel := make ( chan float64 , numberOfTasks )
	for i := 0 ; i < numberOfTasks ; i++ { go processSlice ( i , sliceSize , delta , channel ) }
	sum := float64 ( 0.0 )
	for i := 0 ; i < numberOfTasks ; i++ { sum += <- channel }
	pi := 4.0 * sum * delta
	elapseTime := float64 ( time.Nanoseconds ( ) - startTime ) / 1e9
	fmt.Printf ( "==== Go Parallel pi = %.18f\n" , pi ) ;
	fmt.Printf ( "==== Go Parallel iteration count = %d\n" ,  n ) ;
	fmt.Printf ( "==== Go Parallel elapse = %f\n" , elapseTime ) ;
	fmt.Printf ( "==== Go Parallel threadCount = %d\n" , numberOfTasks )
}

func main ( ) {
	execute ( 1 )
	fmt.Printf ( "\n" )
	execute ( 2 )
	fmt.Printf ( "\n" )
	execute ( 8 )
	fmt.Printf ( "\n" )
	execute ( 32 )
}
