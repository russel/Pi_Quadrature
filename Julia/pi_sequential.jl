#! /usr/bin/env julia

#  Calculation of π using quadrature. Sequential algorithm using a for loop.
#
#  Copyright © 2012–2013  Russel Winder

require("output.jl")

function execute()
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sum = 0.0
    for i = 1:(n + 1)
        sum += 1.0 / (1.0 + ((i - 0.5) * delta) ^ 2)
    end
    pi = 4.0 * delta * sum
    elapseTime = time() - startTime
    out("pi_julia_sequential", pi, n, elapseTime)
end

execute()

# No observable warm-up effect for this code.
