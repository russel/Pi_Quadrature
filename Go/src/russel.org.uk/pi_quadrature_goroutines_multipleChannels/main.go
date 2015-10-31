//  A Go program to calculate π using quadrature employing a parallel algorithm with goroutines and multiple
//  channels.
package main

//  Copyright © 2010–2013, 2015  Russel Winder

import (
	"runtime"
	"russel.org.uk/pi_quadrature_output"
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
	close(channel)
}

func sumItems(channels []chan float64) float64 {
	sum := 0.0
	for _, c := range channels {
		sum += <-c
	}
	return sum
}

func execute(numberOfTasks int) {
	const n = 1000000000
	const delta = 1.0 / float64(n)
	startTime := time.Now()
	runtime.GOMAXPROCS(numberOfTasks)
	sliceSize := n / numberOfTasks
	channels := make([]chan float64, numberOfTasks)
	for i := 0; i < len(channels); i++ {
		channels[i] = make(chan float64)
		go processSlice(i, sliceSize, delta, channels[i])
	}
	pi := 4.0 * delta * sumItems(channels)
	elapseTime := time.Now().Sub(startTime)
	output.OutN("Goroutines Multiple Channels", pi, n, elapseTime, numberOfTasks)
}

func main() {
	execute(1)
	execute(2)
	execute(8)
	execute(32)
}
