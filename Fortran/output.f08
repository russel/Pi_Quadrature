!  Output functions for the Fortran programs to calculate π using quadrature.
!
!  Copyright © 2012, 2014, 2016  Russel Winder

subroutine out(banner, pi_, n, elapseTime)
  integer, parameter:: dp = kind(0.d0)
  character(len=*), intent(in):: banner
  integer, intent(in):: n
  real(dp), intent(in):: elapseTime, pi_
  print '(2(a))', "================================= ", banner
  print '(a, f20.18)', "    π = ", pi_
  print '(a, i0)', "    iteration count = ", n
  print '(a, f10.8)', "    elapse time = ", elapseTime
end subroutine out

subroutine outn(banner, pi_, n, elapseTime, nProcessors)
  integer, parameter:: dp = kind(0.d0)
  character(len=*), intent(in):: banner
  integer, intent(in):: n , nProcessors
  real (dp), intent(in):: elapseTime, pi_
  call out(banner, pi_, n, elapseTime)
  print '(a, i0)', "    processor count = ",  nProcessors
end subroutine outn
