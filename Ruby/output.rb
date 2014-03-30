# -*- coding: utf-8; -*-

#  Output functions for the Ruby codes.
#
#  Copyright © 2012, 2014  Russel Winder

def out(banner, pi, n, elapseTime)
  puts("================ " + banner.to_s)
  puts("\tπ = " + pi.to_s)
  puts("\titeration count = " + n.to_s)
  puts("\telapse time = " + elapseTime.to_s)
end

def outn(banner, pi, n, elapseTime, numberOfTasks)
  out( banner, pi, n, elapseTime)
  puts("\tnumber of tasks = " + numberOfTasks.to_s)
end
