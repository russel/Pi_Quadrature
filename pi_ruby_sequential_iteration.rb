#! /usr/bin/env ruby
# -*- coding: utf-8 -*-

#  Calculation of Pi using quadrature. Sequential algorithm.
#
#  Copyright © 2008–2009,2011 Russel Winder

require 'time'

n = 10000000 #  100 times fewer due to speed issues.
delta = 1.0 / n
startTime = Time.now
sum = 0.0
i = 1
while i <= n do
  x = ( i - 0.5 ) * delta
  sum += 1.0 / ( 1.0 + x * x )
  i += 1
end
pi = 4.0 * delta * sum
elapseTime = Time.now - startTime
puts( "==== Ruby Sequential Iteration pi = " + pi.to_s )
puts( "==== Ruby Sequential Iteration iteration count = " + n.to_s )
puts( "==== Ruby Sequential Iteration elapse = " + elapseTime.to_s )
