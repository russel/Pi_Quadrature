#!/usr/bin/env julia

#  Calculation of π using quadrature. Use a batched parallel map.
#
#  Copyright © 2012–2015  Russel Winder

require("output.jl")

addprocs(CPU_CORES)

@everywhere using Functions: partialSum

function execute(taskCount)
    n = 1000000#000
    delta = 1.0 / n
    startTime = time()
    sliceSize::Int = n / taskCount
    pi = 4.0 * delta * sum(pmap(partialSum, [(i, sliceSize, delta) for i = 1:taskCount]))
    elapseTime = time() - startTime
    out("Parallel Pmap Batched", pi, n, elapseTime, taskCount)
end

println("There is a noticeable 'warm-up' effect for this code so ignore the first two runs.")
execute(32)
execute(32)
execute(1)
execute(2)
execute(8)
execute(32)
