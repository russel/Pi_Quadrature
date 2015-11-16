#!/usr/bin/env julia

#  Calculation of π using quadrature. Sequential algorithm using a for loop.
#
#  Copyright © 2012–2015  Russel Winder

require("output.jl")

# Write this as a function that gets called so as to get the JIT to operate. If just the sequence of
# statements is executed as a script (which is perfectly legal code), it gets interpreted and takes an
# absolute age.

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
    out("Sequential For", pi, n, elapseTime)
end

# No apparent 'warm up' effect.
#execute()
#execute()
#execute()
execute()
