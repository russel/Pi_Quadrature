//  A Go program to calculate π using quadrature employing a sequential algorithm.
package main

//  Copyright © 2010–2013, 2015  Russel Winder

import (
	"russel.org.uk/pi_quadrature_output"
	"time"
)

func main() {
	const n = 1000000000
	const delta = 1.0 / float64(n)
	startTime := time.Now()
	sum := float64(0.0)
	for i := 1; i <= n; i++ {
		x := (float64(i) - 0.5) * delta
		sum += 1.0 / (1.0 + x*x)
	}
	pi := 4.0 * delta * sum
	elapseTime := time.Now().Sub(startTime)
	output.Out("Sequential", pi, n, elapseTime)
}
