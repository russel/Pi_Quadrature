!  Output functions for the Fortran programs to calculate π using quadrature.
!
!  Copyright © 2012, 2014  Russel Winder

subroutine out(banner, pi_, n, elapseTime)
  character (len = 10):: banner
  integer:: n
  real (selected_real_kind (p = 15)):: elapseTime, pi_
  write (*, *) "================================= Fortran ", banner
  write (*, *) "    π =", pi_
  write (*, *) "    iteration count =", n
  write (*, *) "    elapse time =", elapseTime
endsubroutine out

subroutine outn(banner, pi_, n, elapseTime, nProcessors)
  character (len = 10):: banner
  integer:: n , nProcessors
  real (selected_real_kind (p = 15)):: elapseTime, pi_
  call out(banner, pi_, n, elapseTime)
  write (*, *) "    processor count =",  nProcessors
endsubroutine outn
