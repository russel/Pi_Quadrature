//  A Go program to calculate π using quadrature as a parallel algorithm employing goroutines and a single
//  channel.
//
//  Copyright © 2010–2013 Russel Winder

package main

import (
	"./output"
	"runtime"
	"time"
)

func processSlice(id int, sliceSize int, delta float64, channel chan float64) {
	start := 1 + id*sliceSize
	end := (id + 1) * sliceSize
	sum := float64(0.0)
	for i := start; i <= end; i++ {
		x := (float64(i) - 0.5) * delta
		sum += 1.0 / (1.0 + x*x)
	}
	channel <- sum
}

func execute(numberOfTasks int) {
	const n = 1000000000
	const delta = 1.0 / float64(n)
	startTime := time.Now()
	runtime.GOMAXPROCS(numberOfTasks)
	sliceSize := n / numberOfTasks
	channel := make(chan float64, numberOfTasks)
	for i := 0; i < numberOfTasks; i++ {
		go processSlice(i, sliceSize, delta, channel)
	}
	sum := float64(0.0)
	for i := 0; i < numberOfTasks; i++ {
		sum += <-channel
	}
	pi := 4.0 * delta * sum
	elapseTime := time.Now().Sub(startTime)
	output.OutP("Goroutines Single Channel", pi, n, elapseTime, numberOfTasks)
}

func main() {
	execute(1)
	execute(2)
	execute(8)
	execute(32)
}
