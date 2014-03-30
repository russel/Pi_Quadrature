#! /usr/bin/env jruby
# -*- coding: utf-8; -*-

#  Calculation of π using quadrature. Sequential algorithm.
#
#  Copyright © 2008–2009, 2011, 2012, 2014  Russel Winder

require 'time'

load 'output.rb'

n = 10000000 #  100 times fewer due to speed issues.
delta = 1.0 / n
startTime = Time.now
sum = 0.0
(1..n).each{|i|
  x = (i - 0.5) * delta
  sum += 1.0 / (1.0 + x * x)
}
pi = 4.0 * delta * sum
elapseTime = Time.now - startTime
out __FILE__, pi, n, elapseTime
