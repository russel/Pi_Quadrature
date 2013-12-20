#! /usr/bin/env python3

#  Calculation of π using quadrature.  Using PyOpenCL.
#
#  Copyright © 2012  Russel Winder

from time import time

from pyopencl import create_some_context, CommandQueue, Program, Buffer, mem_flags, enqueue_read_buffer

import numpy

from output import out

def execute(workerCount):
    n = 1000000000
    delta = 1.0 / n
    startTime = time()
    sliceSize = n // workerCount
    context = create_some_context()
    queue = CommandQueue(context)
    with open('processSlice_opencl.cl', 'r') as f:
        kernel = Program(context, f.read()).build()
    results = numpy.array(range(workerCount), dtype=numpy.float64)
    buffer = Buffer(context, mem_flags.WRITE_ONLY, results.nbytes)
    kernel.processSlice(queue, results.shape, None, numpy.int32(sliceSize), numpy.float64(delta), buffer)
    enqueue_read_buffer(queue, buffer, results).wait()
    pi = 4.0 * delta * numpy.sum(results)
    elapseTime = time() - startTime
    out(__file__, pi, n, elapseTime)

if __name__ == '__main__':
    execute(1)
    execute(2)
    execute(8)
    execute(32)
