function out(name, pi, n, elapseTime)
    println("================ ", name)
    println("\tπ = " , pi)
    println("\titeration count = " , n)
    println("\telapse time = ", elapseTime)
    println("\tprocessor count = ", nprocs())
end
