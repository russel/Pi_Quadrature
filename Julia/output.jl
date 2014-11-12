#  Output functions to support calculation of π using quadrature.
#
#  Copyright © 2012–2013  Russel Winder

function out(name, pi, n, elapseTime)
    println("================ ", name)
    println("\tπ = ", pi)
    println("\titeration count = ", n)
    println("\telapse time = ", elapseTime)
    println("\tprocessor count = ", CPU_CORES)
end

function out(name, pi, n, elapseTime, taskCount)
    out("$name: task count = $taskCount", pi, n, elapseTime)
end
