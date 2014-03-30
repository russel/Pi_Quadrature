#! /usr/bin/env jruby
# -*- coding: utf-8; -*-

#  Calculation of π using quadrature. Thread-based algorithm.
#
#  Copyright © 2008–2009, 2011, 2012, 2014  Russel Winder

require 'thread'
require 'time'

load 'output.rb'
load 'partialSum.rb'

def execute(threadCount)
  n = 10000000 # 100 times fewer due to speed issues.
  delta = 1.0 / n
  startTime = Time.now
  sliceSize = n / threadCount
  results = Queue.new
  (0...threadCount).each{|i| Thread.new{results.enq partialSum(i, sliceSize, delta)}}
  sum = 0.0
  (0...threadCount).each{sum += results.deq}
  pi = 4.0 * delta * sum
  elapseTime = Time.now - startTime
  outn __FILE__, pi, n, elapseTime, threadCount
end

execute 1
execute 2
execute 8
execute 32
