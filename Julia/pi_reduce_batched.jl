#! /usr/bin/env julia

#  Calculation of π using quadrature. Use a batched parallel map.
#
#  Copyright © 2012–2013  Russel Winder

require("output.jl")

function partialSum(data)
    id = data[0]
    sliceSize = data[1]
    delta = data[2]
    sum = 0.0
    for i = (1 + id * sliceSize):((id + 1) * sliceSize)
        sum += 1.0 / (1.0 + ((i - 0.5) * delta) ^ 2)
    end
    return sum
end

function execute(taskCount)
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sliceSize = n / taskCount
    pi = 4.0 * delta * sum(pmap(partialSum, {(i, sliceSize, delta) for i=1:taskCount}))
    elapseTime = time() - startTime
    out("pi_reduce_batched", pi, n, elapseTime, taskCount)
end

addprocs(CPU_CORES -1)
execute(1)
execute(2)
execute(8)
execute(32)
