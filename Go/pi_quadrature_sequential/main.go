//  A Go program to calculate π using quadrature employing a sequential algorithm.
package main

//  Copyright © 2010–2013, 2015, 2017  Russel Winder

import (
	"pi_quadrature_output"
	"time"
)

func main() {
	const n = 1000000000
	const δ = 1.0 / float64(n)
	t_start := time.Now()
	total := float64(0.0)
	for i := 1; i <= n; i++ {
		x := (float64(i) - 0.5) * δ
		total += 1.0 / (1.0 + x*x)
	}
	π := 4.0 * δ * total
	t_elapse := time.Since(t_start)
	output.Out("Sequential", π, n, t_elapse)
}
