#! /usr/bin/env python3

#  Calculation of π using quadrature. Using PyOpenCL.
#
#  Copyright © 2012, 2014  Russel Winder

from time import time

from pyopencl import create_some_context, CommandQueue, Program, Buffer, mem_flags, enqueue_read_buffer

import numpy

from output import out

n = 1000000000
delta = 1.0 / n
startTime = time()
context = create_some_context()
queue = CommandQueue(context)
with open('processSlice_opencl.cl', 'r') as f:
    kernel = Program(context, f.read()).build()
# Quadro FX 570 card on Anglides only supports 32-bit operations, hence float not double.
results = numpy.array(n, dtype=numpy.float32)
buffer = Buffer(context, mem_flags.WRITE_ONLY, results.nbytes)
kernel.processSlice(queue, results.shape, None, numpy.int32(n), numpy.float32(delta), buffer)
enqueue_read_buffer(queue, buffer, results).wait()
pi = 4.0 * delta * numpy.sum(results)
elapseTime = time() - startTime
out(__file__, pi, n, elapseTime)
