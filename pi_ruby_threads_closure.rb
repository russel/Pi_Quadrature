#! /usr/bin/env jruby
# -*- coding: utf-8; -*-

#  Calculation of Pi using quadrature. Thread-based algorithm.
#
#  Copyright © 2008–2009,2011,2012 Russel Winder

require 'time'
require 'thread'
load 'output.rb'

def partialSum ( id , sliceSize , delta )
  sum = 0.0
  ( ( 1 + id * sliceSize ) .. ( ( id + 1 ) * sliceSize + 1 ) ).each { | i |
    x = ( i - 0.5 ) * delta
    sum += 1.0 / ( 1.0 + x * x )
  }
  sum
end

def execute ( threadCount )
  n = 10000000 # 100 times fewer due to speed issues.
  delta = 1.0 / n
  startTime = Time.now
  sliceSize = n / threadCount
  results = Queue.new
  ( 0 ... threadCount ).each { | i | Thread.new { results.enq partialSum( i , sliceSize , delta ) } }
  sum = 0.0
  ( 0 ... threadCount ).each { sum += results.deq }
  pi = 4.0 * delta * sum
  elapseTime = Time.now - startTime
  out __FILE__ , pi , n , elapseTime , threadCount
end

execute 1
execute 2
execute 8
execute 32
