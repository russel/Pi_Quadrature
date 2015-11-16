#!/usr/bin/env julia

#  Calculation of π using quadrature. Sequential algorithm using a for loop.
#
#  Copyright © 2012–2015  Russel Winder

require("output.jl")

# Write this as a function that gets called so as to get the JIT to operate. If just the sequence of
# statements is executed as a script (which is perfectly legal code), it gets interpreted and takes an
# absolute age.

function execute()
    n = 10000000 # 100 times fewer than canonical for performance reasons.
    delta = 1.0 / n
    startTime = time()
    pi = 4.0 * delta * reduce((t, x) -> t + 1.0 / (1.0 + ((x - 0.5) * delta) ^ 2), 1:(n + 1))
    elapseTime = time() - startTime
    out("Sequential Reduce", pi, n, elapseTime)
end

# No apparent 'warm up' effect.
#execute()
#execute()
#execute()
execute()
