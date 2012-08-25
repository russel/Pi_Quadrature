#! /usr/bin/env julia

#  Calculation of π using quadrature. Sequential algorithm using a for loop.
#
#  Copyright © 2012 Russel Winder

load("output.jl")

n = 1000000000  # 100 times fewer than C due to speed issues.
delta = 1.0 / n
startTime = time()
# Suspect parallel for with reduction doesn't quite work as anticipated.
sum = @parallel (+) for i = 1:(n + 1)
    1.0 / (1.0 + ((i - 0.5) * delta) ^ 2)
end
pi = 4.0 * delta * sum
elapseTime = time() - startTime
out("pi_julia_reduce", pi, n, elapseTime)
