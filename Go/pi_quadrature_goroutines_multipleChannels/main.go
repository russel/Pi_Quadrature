//  A Go program to calculate π using quadrature employing a parallel algorithm with goroutines and multiple
//  channels.
package main

//  Copyright © 2010–2013, 2015, 2017  Russel Winder

import (
	"pi_quadrature_output"
	"runtime"
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
	close(channel)
}

func Σ(channels []chan float64) float64 {
	total := 0.0
	for _, c := range channels {
		total += <-c
	}
	return total
}

func execute(numberOfTasks int) {
	const n = 1000000000
	const δ = 1.0 / float64(n)
	sliceSize := n / numberOfTasks
	channels := make([]chan float64, numberOfTasks)
	runtime.GOMAXPROCS(numberOfTasks)
	t_start := time.Now()
	for i := 0; i < len(channels); i++ {
		channels[i] = make(chan float64)
		go calculatePartialSum(i, sliceSize, δ, channels[i])
	}
	π := 4.0 * δ * Σ(channels)
	t_elapse := time.Since(t_start)
	output.OutN("Goroutines Multiple Channels", π, n, t_elapse, numberOfTasks)
}

func main() {
	execute(1)
	execute(2)
	execute(8)
	execute(32)
}
