//  A Go program to calculate π using quadrature employing a parallel algorithm with goroutines and a single
//  channel.
package main

//  Copyright © 2010–2013, 2015, 2017  Russel Winder

import (
	"runtime"
	"pi_quadrature_output"
	"time"
)

func calculatePartialSum(id int, sliceSize int, δ float64, channel chan float64) {
	start := 1 + id*sliceSize
	end := (id + 1) * sliceSize
	total := float64(0.0)
	for i := start; i <= end; i++ {
		x := (float64(i) - 0.5) * δ
		total += 1.0 / (1.0 + x*x)
	}
	channel <- total
}

func Σ(channel chan float64, numberOfTasks int) float64 {
	total := 0.0
	for i := 0; i < numberOfTasks; i++ {
		total += <-channel
	}
	return total
}

func execute(numberOfTasks int) {
	const n = 1000000000
	const δ = 1.0 / float64(n)
	t_start := time.Now()
	runtime.GOMAXPROCS(numberOfTasks)
	sliceSize := n / numberOfTasks
	channel := make(chan float64, numberOfTasks)
	for i := 0; i < numberOfTasks; i++ {
		go calculatePartialSum(i, sliceSize, δ, channel)
	}
	π := 4.0 * δ * Σ(channel, numberOfTasks)
	t_elapse := time.Since(t_start)
	output.OutN("Goroutines Single Channel", π, n, t_elapse, numberOfTasks)
}

func main() {
	execute(1)
	execute(2)
	execute(8)
	execute(32)
}
