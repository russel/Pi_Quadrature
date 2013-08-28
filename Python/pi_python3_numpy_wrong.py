#! /usr/bin/env python3

#  Calculation of π using quadrature. Sequential algorithm using NumPy – sort of. This is an example of how
#  not to use NumPy.
#
#  Copyright © 2008–2013  Russel Winder

from numpy import int, double, add, subtract, multiply, divide
from output import out
from time import time

if __name__ == '__main__':
    n = int(1000000)  # 1000 times fewer than C due to speed issues.
    delta = divide(1.0, n)
    startTime = time()
    sum = double(0.0)
    for i in range(1, n + 1):
        x = multiply(subtract(i, 0.5), delta)
        sum = add(sum, divide(1.0, add(1.0, multiply(x, x))))
    pi = multiply(4.0, multiply(delta, sum))
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)
