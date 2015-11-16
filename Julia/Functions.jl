#  Calculation of π using quadrature. The partial dum function for the batched pmap version.
#
#  Copyright © 2012–2015  Russel Winder

module Functions

export partialSum

function partialSum(data)
    id = data[1] - 1
    sliceSize = data[2]
    delta = data[3]
    # For loops are much faster than using reduce in Julia :-(
    sum = 0.0
    for i = (1 + id * sliceSize):((id + 1) * sliceSize)
        sum += 1.0 / (1.0 + ((i - 0.5) * delta) ^ 2)
    end
    return sum
end

end
