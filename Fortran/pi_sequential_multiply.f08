!  A Fortran program to calculate π using quadrature as a sequential algorithm.
!
!  Copyright © 2008–2012, 2014, 2016  Russel Winder

program pi
  implicit none
  integer, parameter:: n = 1000000000
  integer, parameter:: dp = kind(0.d0)
  real(dp), parameter:: delta = 1.0_dp / n
  real(dp):: sum = 0.0_dp, x, elapseTime, pi_
  integer:: i, startTime, startFrequency, endTime, endFrequency
  call system_clock(startTime, startFrequency)
  do i = 1, n
     x = (i - 0.5_dp) * delta
     sum = sum + 1.0_dp / (1.0_dp + x * x)
  end do
  pi_ = 4.0_dp * delta * sum
  call system_clock(endTime, endFrequency)
  elapseTime = endTime - startTime
  elapseTime = elapseTime / startFrequency
  call out("Sequential Multiply", pi_, n, elapseTime)
end program pi
