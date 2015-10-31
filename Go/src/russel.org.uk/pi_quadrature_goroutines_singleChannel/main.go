//  A Go program to calculate π using quadrature employing a parallel algorithm with goroutines and a single
//  channel.
package main

//  Copyright © 2010–2013, 2015  Russel Winder

import (
	"runtime"
	"russel.org.uk/pi_quadrature_output"
	"time"
)

func calculatePartialSum(id int, sliceSize int, delta float64, channel chan float64) {
	start := 1 + id*sliceSize
	end := (id + 1) * sliceSize
	sum := float64(0.0)
	for i := start; i <= end; i++ {
		x := (float64(i) - 0.5) * delta
		sum += 1.0 / (1.0 + x*x)
	}
	channel <- sum
}

func sumFirstNItems(channel chan float64, numberOfTasks int) float64 {
	sum := 0.0
	for i := 0; i < numberOfTasks; i++ {
		sum += <-channel
	}
	return sum
}

func execute(numberOfTasks int) {
	const n = 1000000000
	const delta = 1.0 / float64(n)
	startTime := time.Now()
	runtime.GOMAXPROCS(numberOfTasks)
	sliceSize := n / numberOfTasks
	channel := make(chan float64, numberOfTasks)
	for i := 0; i < numberOfTasks; i++ {
		go calculatePartialSum(i, sliceSize, delta, channel)
	}
	pi := 4.0 * delta * sumFirstNItems(channel, numberOfTasks)
	elapseTime := time.Now().Sub(startTime)
	output.OutN("Goroutines Single Channel", pi, n, elapseTime, numberOfTasks)
}

func main() {
	execute(1)
	execute(2)
	execute(8)
	execute(32)
}
