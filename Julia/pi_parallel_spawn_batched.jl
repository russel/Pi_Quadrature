#!/usr/bin/env julia

#  Calculation of π using quadrature. Use a scatter gather using spawn.
#
#  Copyright © 2012–2015  Russel Winder

require("output.jl")

addprocs(CPU_CORES)

@everywhere import Functions: partialSum

function execute(taskCount)
    n = 1000000#000
    delta = 1.0 / n
    startTime = time()
    sliceSize = n / taskCount
    pi = 4.0 * delta * sum([fetch(x) for x in [@spawn partialSum((i, sliceSize, delta)) for i = 1:taskCount]])
    elapseTime = time() - startTime
    out("Parallel Spawn Batched", pi, n, elapseTime, taskCount)
end

println("There is a noticeable 'warm-up' effect for this code so ignore the first two runs.")
execute(32)
execute(32)
execute(1)
execute(2)
execute(8)
execute(32)
