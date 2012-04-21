/* 
 *  A Go program to calculate Pi using quadrature as a potentially parallel algorithm
 *  employing goroutines and channels.
 *
 *  This is an embarrassingly parallel (i.e. scatter/gather) algorithm so the more processors
 *  you have, the faster it can go, hopefully speed-up is linear.  Obviously if you use more
 *  processes than you have processors there will be no extra speed up.
 *
 *  The number of iterations used here is probably far more than is needed to cause the
 *  accuracy of the result not to be representable in the hardware floating point numbers
 *  being employed.
 *
 *  Copyright © 2010–2012 Russel Winder
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
	startTime := time.Now ( )
	runtime.GOMAXPROCS ( numberOfTasks ) // Temporary hack
	sliceSize := n / numberOfTasks
	channel := make ( chan float64 , numberOfTasks )
	for i := 0 ; i < numberOfTasks ; i++ { go processSlice ( i , sliceSize , delta , channel ) }
	sum := float64 ( 0.0 )
	for i := 0 ; i < numberOfTasks ; i++ { sum += <- channel }
	pi := 4.0 * delta * sum
	elapseTime := time.Now ( ).Sub ( startTime )
	fmt.Printf ( "==== Go Parallel pi = %.18f\n" , pi ) ;
	fmt.Printf ( "==== Go Parallel iteration count = %d\n" ,  n ) ;
	fmt.Printf ( "==== Go Parallel elapse = %v\n" , elapseTime ) ;
	fmt.Printf ( "==== Go Parallel threadCount = %d\n" , numberOfTasks )
}

func main ( ) {
	execute ( 1 )
	fmt.Println ( )
	execute ( 2 )
	fmt.Println ( )
	execute ( 8 )
	fmt.Println ( )
	execute ( 32 )
}
