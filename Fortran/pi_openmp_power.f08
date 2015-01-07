!  A Fortran program to calculate π using quadrature as a parallel algorithm using OpenMP.
!
!  Copyright © 2008–2012, 2014  Russel Winder

program pi
  implicit none
  integer, parameter:: n = 1000000000
  integer, parameter:: DoubleKind = selected_real_kind(p = 15)
  real (DoubleKind), parameter:: delta = 1.0 / n
  real (DoubleKind):: sum = 0.0, elapseTime, pi_
  integer:: i, startTime, startFrequency, endTime, endFrequency, omp_get_num_procs
  call system_clock(startTime, startFrequency)
!$omp parallel do private(i) reduction(+ : sum)
  do i = 1, n
     sum = sum + 1.0 / (1.0 + ((i - 0.5) * delta) ** 2)
  enddo
!$omp end parallel do
  pi_ = 4.0 * delta * sum
  call system_clock(endTime, endFrequency)
  elapseTime = endTime - startTime
  elapseTime = elapseTime / startFrequency
  call outn("OpenMP", pi_, n, elapseTime, omp_get_num_procs())
endprogram pi
