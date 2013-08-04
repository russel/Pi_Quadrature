#! /usr/bin/env julia

#  Calculation of π using quadrature. Using a parallel for loop.
#
#  Copyright © 2012–2013  Russel Winder

require("output.jl")

# For some reason(s) the parallel for code loops infinitely unless it is in a function.

function execute()
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sum = @parallel (+) for i = 1:(n + 1)
        1.0 / (1.0 + ((i - 0.5) * delta) ^ 2)
    end
    pi = 4.0 * delta * sum
    elapseTime = time() - startTime
    out("pi_reduce", pi, n, elapseTime)
end

addprocs(CPU_CORES -1)
println("There is a noticeable 'warm-up' effect for this code so only take the second run.")
execute()
execute()
