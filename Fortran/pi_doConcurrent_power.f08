!  A Fortran program to calculate π using quadrature as a parallel algorithm using OpenMP but via the
!  Fortran 2008 generic syntax.
!
!  Copyright © 2008–2012, 2014, 2016  Russel Winder

program pi
  implicit none
  integer, parameter:: n = 1000000000
  integer, parameter:: dp = kind(0.d0)
  real(dp), parameter:: delta = 1.0_dp / n
  real(dp):: sum = 0.0_dp, elapseTime, pi_
  integer:: i, startTime, startFrequency, endTime, endFrequency, omp_get_num_procs
  call system_clock(startTime, startFrequency)
  do concurrent(i = 1: n)
     sum = sum + 1.0_dp / (1.0_dp + ((i - 0.5_dp) * delta) ** 2)
  end do
  pi_ = 4.0_dp * delta * sum
  call system_clock(endTime, endFrequency)
  elapseTime = endTime - startTime
  elapseTime = elapseTime / startFrequency
  call outn("Do Concurrent", pi_, n, elapseTime, omp_get_num_procs())
end program pi
