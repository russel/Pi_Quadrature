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
	"time"
	"runtime"
	"./output"
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
	close ( channel )
}

func execute ( numberOfTasks int ) {
	const n = 1000000000
	const delta = 1.0 / float64 ( n )
	startTime := time.Now ( )
	runtime.GOMAXPROCS ( numberOfTasks ) // Temporary hack
	sliceSize := n / numberOfTasks
	channels := make ( [ ] chan float64 , numberOfTasks )
	for i := 0 ; i < numberOfTasks ; i++ {
		channels[i] = make ( chan float64 )
		go processSlice ( i , sliceSize , delta , channels[i] )
	}
	sum := float64 ( 0.0 )
	for i := range channels { sum += <- channels[i] }
	pi := 4.0 * delta * sum
	elapseTime := time.Now ( ).Sub ( startTime )
	output.OutP ( "Go Goroutines Multiple Channels", pi , n , elapseTime , numberOfTasks )
}

func main ( ) {
	execute ( 1 )
	execute ( 2 )
	execute ( 8 )
	execute ( 32 )
}
