!  A Fortran program to calculate π using quadrature as a sequential algorithm.
!
!  Copyright © 2008–2012, 2014  Russel Winder

program pi
  implicit none
  integer, parameter:: n = 1000000000
  integer, parameter:: DoubleKind = selected_real_kind (p = 15)
  real (DoubleKind), parameter:: delta = 1.0 / n
  real (DoubleKind):: sum = 0.0, x, elapseTime, pi_
  integer:: i, startTime, startFrequency, endTime, endFrequency
  call system_clock(startTime, startFrequency)
  do i = 1, n
     x = (i - 0.5) * delta
     sum = sum + 1.0 / (1.0 + x * x)
  enddo
  pi_ = 4.0 * delta * sum
  call system_clock(endTime, endFrequency)
  elapseTime = endTime - startTime
  elapseTime = elapseTime / startFrequency
  call out("Sequential Multiply", pi_, n, elapseTime)
endprogram pi
