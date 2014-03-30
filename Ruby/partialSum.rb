# -*- coding: utf-8; -*-

#  The partial sum function for the Ruby codes.
#
#  Copyright © 2012, 2014  Russel Winder

def partialSum(id, sliceSize, delta)
  sum = 0.0
  ((1 + id * sliceSize)..((id + 1) * sliceSize + 1)).each{|i|
    x = (i - 0.5) * delta
    sum += 1.0 / (1.0 + x * x)
  }
  sum
end
