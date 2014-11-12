#! /usr/bin/env julia

#  Calculation of π using quadrature. Using a parallel for reduction.
#
#  Copyright © 2012–2014  Russel Winder

require("output.jl")

# Write this as a function that gets called so as to get the JIT to operate. If just the sequence of
# statements is executed as a script (which is perfectly legal code), it gets interpreted and takes an
# absolute age. Actually we need to call this twice anyway because there is a JIT warm up issue.

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

addprocs(CPU_CORES)
println("There is a noticeable 'warm-up' effect for this code so only take the second run.")
execute()
execute()
